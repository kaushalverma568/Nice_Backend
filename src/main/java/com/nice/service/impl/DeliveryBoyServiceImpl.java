package com.nice.service.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;

import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.AssetConstant;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.Role;
import com.nice.constant.UserOtpTypeEnum;
import com.nice.constant.UserType;
import com.nice.dto.DeliveryBoyAccountDetailsDTO;
import com.nice.dto.DeliveryBoyDTO;
import com.nice.dto.DeliveryBoyPersonalDetailsDTO;
import com.nice.dto.DeliveryBoyResponseDTO;
import com.nice.dto.Notification;
import com.nice.dto.UserOtpDto;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.jms.queue.JMSQueuerService;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.DeliveryBoyMapper;
import com.nice.model.DeliveryBoy;
import com.nice.model.DeliveryBoyLocation;
import com.nice.model.DeliveryBoySendNotificationHistory;
import com.nice.model.UserLogin;
import com.nice.model.UserOtp;
import com.nice.model.Vendor;
import com.nice.repository.DeliveryBoyRepository;
import com.nice.repository.DeliveryBoySendNotificationHistoryRepository;
import com.nice.service.AssetService;
import com.nice.service.DeliveryBoyLocationService;
import com.nice.service.DeliveryBoyService;
import com.nice.service.FileStorageService;
import com.nice.service.OtpService;
import com.nice.service.UserLoginService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;
import com.nice.util.ExportCSV;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 24-Mar-2020
 * @description :
 */

@Transactional(rollbackFor = Throwable.class)
@Service("deliveryBoyService")
public class DeliveryBoyServiceImpl implements DeliveryBoyService {

	private static final Logger LOGGER = LoggerFactory.getLogger(DeliveryBoyServiceImpl.class);

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private UserLoginService userLoginService;

	@Autowired
	private VendorService vendorService;
	
	@Autowired
	private ExportCSV exportCSV;
	
	@Autowired
	private DeliveryBoyRepository deliveryBoyRepository;

	@Autowired
	private DeliveryBoyMapper deliveryBoyMapper;

	@Autowired
	private DeliveryBoyLocationService deliveryBoyLocationService;

	@Autowired
	private DeliveryBoySendNotificationHistoryRepository deliveryBoySendNotificationHistoryRepository;

	@Autowired
	private AssetService assetService;

	@Autowired
	private OtpService otpService;

	@Autowired
	private FileStorageService fileStorageService;

	@Autowired
	private JMSQueuerService jmsQueuerService;

	@Override
	public void addDeliveryBoy(final DeliveryBoyDTO deliveryBoyDTO, final MultipartFile profilePicture) throws ValidationException, NotFoundException {
		DeliveryBoy deliveryBoy = deliveryBoyMapper.toEntity(deliveryBoyDTO);

		/**
		 * Check if delivery boy already exists, if so then lets only send him email
		 * again.
		 */
		Optional<DeliveryBoy> optDeliveryBoy = deliveryBoyRepository.findByEmailIgnoreCase(deliveryBoyDTO.getEmail().toLowerCase());
		if (optDeliveryBoy.isPresent() && !optDeliveryBoy.get().getIsEmailVerified().booleanValue()) {
			deliveryBoy = optDeliveryBoy.get();
			Optional<UserLogin> optUserLogin = userLoginService.getUserLoginBasedOnEmailAndRole(deliveryBoyDTO.getEmail().toLowerCase(),
					Role.DELIVERY_BOY.name());
			if (optUserLogin.isPresent()) {
				sendOtpForEmailVerification(optUserLogin.get(), deliveryBoy);
				return;
			}
		}

		uploadImage(profilePicture, deliveryBoy);
		deliveryBoy.setIsEmailVerified(false);
		/**
		 * it will be true when he will logged in
		 */
		deliveryBoy.setIsLogin(false);
		/**
		 * it will be true when he is going for delivery
		 */
		deliveryBoy.setIsBusy(false);
		deliveryBoy.setIsPhoneNumberVerified(false);
		deliveryBoy.setActive(false);
		deliveryBoy.setNoOfRating(0L);
		deliveryBoy.setRating(0D);
		deliveryBoy = deliveryBoyRepository.save(deliveryBoy);
		/**
		 * set login details of delivery boy
		 */
		final UserLogin userLogin = new UserLogin();
		userLogin.setEntityId(deliveryBoy.getId());
		userLogin.setEntityType(UserType.DELIVERY_BOY.name());
		userLogin.setEmail(deliveryBoy.getEmail());
		userLogin.setRole(Role.DELIVERY_BOY.name());
		userLogin.setPassword(deliveryBoyDTO.getPassword());
		userLogin.setActive(false);
		userLoginService.addUserLogin(userLogin);
		LOGGER.info("Inside add DeliveryBoy service deliveryBoy:{}", deliveryBoy);

		/**
		 * Code to generate OTP and send that in email.
		 */
		sendOtpForEmailVerification(userLogin, deliveryBoy);

	}

	@Override
	public String updatePersonalDetails(final DeliveryBoyPersonalDetailsDTO deliveryBoyPersonalDetailsDTO) throws NotFoundException, ValidationException {
		DeliveryBoy deliveryBoy = getDeliveryBoyDetail(deliveryBoyPersonalDetailsDTO.getId());
		BeanUtils.copyProperties(deliveryBoyPersonalDetailsDTO, deliveryBoy);
		String userName = null;
		if (!deliveryBoy.getEmail().equalsIgnoreCase(deliveryBoyPersonalDetailsDTO.getEmail())) {
			/**
			 *
			 * if delivery boy is out for delivery then he can not update his email
			 */
			if (deliveryBoy.getIsBusy().booleanValue()) {
				throw new ValidationException(messageByLocaleService.getMessage("assigned.order.exist", null));
			} else {
				/**
				 * when delivery boy updates an email then de active him until email not
				 * verified and send verification link
				 */
				deliveryBoy.setIsEmailVerified(false);
				deliveryBoy.setActive(false);
				deliveryBoy.setIsLogin(false);

				final Optional<UserLogin> userLogin = userLoginService.getUserLoginBasedOnEmailAndRole(deliveryBoy.getEmail().toLowerCase(),
						Role.DELIVERY_BOY.name());
				if (userLogin.isPresent()) {
					userLogin.get().setEmail(deliveryBoyPersonalDetailsDTO.getEmail().toLowerCase());
					userLogin.get().setActive(false);
					userLoginService.updateUserLogin(userLogin.get());
					userName = userLogin.get().getEmail();
					/**
					 * Code to generate OTP and send that in email.
					 */
					sendOtpForEmailVerification(userLogin.get(), deliveryBoy);
				}
			}
		}
		if (!deliveryBoy.getPhoneNumber().equals(deliveryBoyPersonalDetailsDTO.getPhoneNumber())) {
			/**
			 * send otp for contact verification
			 */
			deliveryBoy.setIsPhoneNumberVerified(false);
		}
		deliveryBoyRepository.save(deliveryBoy);
		return userName;
	}

	@Override
	public void updateAccountDetails(final DeliveryBoyAccountDetailsDTO deliveryBoyAccountDetailsDTO) throws NotFoundException {
		DeliveryBoy deliveryBoy = getDeliveryBoyDetail(deliveryBoyAccountDetailsDTO.getId());
		BeanUtils.copyProperties(deliveryBoyAccountDetailsDTO, deliveryBoy);
		deliveryBoyRepository.save(deliveryBoy);
	}

	@Override
	public DeliveryBoyResponseDTO getDeliveryBoy(final Long deliveryBoyId) throws NotFoundException {
		return deliveryBoyMapper.toDto(getDeliveryBoyDetail(deliveryBoyId));
	}

	@Override
	public DeliveryBoy getDeliveryBoyDetail(final Long deliveryBoyId) throws NotFoundException {
		return deliveryBoyRepository.findById(deliveryBoyId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("deliveryboy.not.found", new Object[] { deliveryBoyId })));
	}

	@Override
	public Page<DeliveryBoy> getDeliveryBoyList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final String searchKeyword)
			throws NotFoundException {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("id"));
		if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(searchKeyword)) {
			if (activeRecords != null) {
				return deliveryBoyRepository.findAllByActiveAndFirstNameContainingIgnoreCaseOrLastNameContainingIgnoreCase(activeRecords, searchKeyword,
						searchKeyword, pageable);
			} else {
				return deliveryBoyRepository.findAllByFirstNameContainingIgnoreCaseOrLastNameContainingIgnoreCase(searchKeyword, searchKeyword, pageable);
			}
		} else {
			if (activeRecords != null) {
				return deliveryBoyRepository.findAllByActive(activeRecords, pageable);
			} else {
				return deliveryBoyRepository.findAll(pageable);
			}
		}
	}

	
	@Override
	public void exportList(Boolean activeRecords, HttpServletResponse httpServletResponse) throws IOException {
		List<DeliveryBoy> deliveryBoyList;
		List<DeliveryBoyResponseDTO> deliveryBoyDtoList = new ArrayList<>();
		if (activeRecords != null) {
			deliveryBoyList = deliveryBoyRepository.findAllByActive(activeRecords);
		} else {
			deliveryBoyList = deliveryBoyRepository.findAll();
		}
		for (DeliveryBoy deliveryBoy : deliveryBoyList) {
			deliveryBoyDtoList.add( deliveryBoyMapper.toDto(deliveryBoy));
		}
		final Object[] deliveryBoyHeaderField = new Object[] {"Delivery Boy Name","Email","Gender","Phone Number","Bank Name","Branch Name","Acount Name","Bank Account Number","Kib No","Branch City"};
		final Object[] deliveryBoyDataField = new Object[] { "name","email","gender","phoneNumber","bankName","branchName","accountName","bankAccountNumber","kibNo","branchCity"};
		exportCSV.writeCSVFile(deliveryBoyDtoList, deliveryBoyDataField, deliveryBoyHeaderField, httpServletResponse);
	}
	
	@Override
	public String changeStatus(final Long deliveryBoyId, final Boolean active) throws NotFoundException, ValidationException {
		final DeliveryBoy deliveryBoy = getDeliveryBoyDetail(deliveryBoyId);
		LOGGER.info("Existing DeliveryBoy details {} ", deliveryBoy);
		String userName = null;
		final Optional<UserLogin> userLogin = userLoginService.getUserLoginBasedOnEmailAndRole(deliveryBoy.getEmail(), Role.DELIVERY_BOY.name());
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (deliveryBoy.getActive().equals(active)) {
			throw new ValidationException(messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "deliveryboy.active" : "deliveryboy.deactive", null));
		} else if (userLogin.isPresent()) {
			if (Boolean.FALSE.equals(active)) {
				/**
				 * if delivery boy has assigned orders and not delivered yet then can't deactive
				 */
				if (deliveryBoy.getIsBusy().booleanValue()) {
					throw new ValidationException(messageByLocaleService.getMessage("deactive.assigned.order.exist", null));
				}
				deliveryBoy.setIsLogin(false);
			} else {
				if (!deliveryBoy.getIsEmailVerified().booleanValue()) {
					throw new ValidationException(messageByLocaleService.getMessage("email.not.verified", null));
				}
			}
			userLogin.get().setActive(active);
			userLoginService.updateUserLogin(userLogin.get());
			deliveryBoy.setActive(active);
			deliveryBoyRepository.save(deliveryBoy);
			userName = userLogin.get().getEmail();
		} else {
			throw new NotFoundException(messageByLocaleService.getMessage("user.not.exists.email", new Object[] { deliveryBoy.getEmail() }));
		}
		return userName;
	}

	@Override
	public void updateProfilePicture(final MultipartFile profilePicture, final Long deliveryBoyId) throws NotFoundException, ValidationException {
		DeliveryBoy deliveryBoy = getDeliveryBoyDetail(deliveryBoyId);
		deleteOldImage(deliveryBoy);
		uploadImage(profilePicture, deliveryBoy);
		deliveryBoyRepository.save(deliveryBoy);
	}

	@Override
	public Boolean isDeliveryBoyExists(final DeliveryBoyDTO deliveryBoyDTO) {
		if (deliveryBoyDTO.getId() != null) {
			/**
			 * At the time of update is deliveryBoy with same email exist or not except it's
			 * own id
			 */
			return deliveryBoyRepository.findByEmailIgnoreCaseAndIdNot(deliveryBoyDTO.getEmail(), deliveryBoyDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is deliveryBoy with same email exist or not
			 */
			Optional<DeliveryBoy> optDeliveryboy = deliveryBoyRepository.findByEmailIgnoreCase(deliveryBoyDTO.getEmail());
			if (optDeliveryboy.isPresent()) {
				/**
				 * If the delivery boy is present and his email not verified, then we will be
				 * sending the verification link for him again, if the email is verified then we
				 * will be returning true.
				 */

				return optDeliveryboy.get().getIsEmailVerified();
			} else {
				return false;
			}
		}
	}

	/**
	 * upload profile picture of delivery boy
	 *
	 * @param profilePicture
	 * @param deliveryBoy
	 */
	private void uploadImage(final MultipartFile profilePicture, final DeliveryBoy deliveryBoy) {
		deliveryBoy.setProfilePictureName(assetService.saveAsset(profilePicture, AssetConstant.DELIVERY_BOY, 0));
		deliveryBoy.setProfilePictureOriginalName(profilePicture.getOriginalFilename());
	}

	@Override
	public void verifyEmail(final Long deliveryBoyId) throws NotFoundException {
		DeliveryBoy deliveryBoy = getDeliveryBoyDetail(deliveryBoyId);
		deliveryBoy.setIsEmailVerified(true);
		deliveryBoyRepository.save(deliveryBoy);
	}

	@Override
	public void updateIsLogin(final String userName) throws NotFoundException {
		Optional<UserLogin> optUserLogin = userLoginService.getUserLoginBasedOnEmailAndRole(userName, Role.DELIVERY_BOY.name());
		if (optUserLogin.isPresent()) {
			DeliveryBoy deliveryBoy = getDeliveryBoyDetail(optUserLogin.get().getEntityId());
			deliveryBoy.setIsLogin(true);
			deliveryBoyRepository.save(deliveryBoy);
		} else {
			throw new NotFoundException(messageByLocaleService.getMessage("user.not.exists.email", new Object[] { userName }));
		}
	}

	/**
	 * @param userLogin
	 * @param deliveryBoy
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	private void sendOtpForEmailVerification(final UserLogin userLogin, final DeliveryBoy deliveryBoy) throws NotFoundException, ValidationException {
		UserOtpDto userOtpDto = new UserOtpDto();
		userOtpDto.setEmail(deliveryBoy.getEmail());
		userOtpDto.setType(UserOtpTypeEnum.EMAIL.name());
		userOtpDto.setUserLoginId(userLogin.getId());
		UserOtp otp = otpService.generateOtp(userOtpDto);

		sendEmail(otp.getOtp(), userLogin.getId(), deliveryBoy.getEmail());
	}

	private void sendEmail(final String otp, final Long userId, final String email) {
		Notification notification = new Notification();
		notification.setOtp(otp);
		notification.setUserId(userId);
		notification.setEmail(email);
		notification.setType(NotificationQueueConstants.EMAIL_VERIFICATION);
		jmsQueuerService.sendEmail(NotificationQueueConstants.NON_NOTIFICATION_QUEUE, notification);
	}

	/**
	 * delete old profile picture
	 *
	 * @param deliveryBoy
	 */
	private void deleteOldImage(final DeliveryBoy deliveryBoy) {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(deliveryBoy.getProfilePictureName())) {
			fileStorageService.deleteFile(deliveryBoy.getProfilePictureName(), AssetConstant.DELIVERY_BOY);
		}
	}

	@Override
	public List<Long> getNextThreeNearestDeliveryBoysFromVendor(final Long orderId, final Long vendorId) throws NotFoundException {
		Vendor vendor = vendorService.getVendorDetail(vendorId);
		/**
		 * get all delivery boys who is logged in, not busy with any orders and has not
		 * sended notification before
		 */
		List<DeliveryBoy> availableDeliveryBoys = deliveryBoyRepository.getAllNextAvailableDeliveryBoys();
		List<DeliveryBoy> busyDeliveryBoys = new ArrayList<>();
		/**
		 * if idle delivery boys is not available then go for a busy delivery boys who
		 * is going for delivery of orders(not for replacement or return) and at a time
		 * assigned order count is 1
		 */
		if (availableDeliveryBoys.isEmpty()) {
			busyDeliveryBoys = deliveryBoyRepository.getAllNextAvailableDeliveryBoysOnBusyTime(orderId);
		}
		availableDeliveryBoys.addAll(busyDeliveryBoys);
		Map<Long, Double> deliveryBoyWithDistanceMap = new HashMap<>();
		List<Long> nearestDeliveryBoys = new ArrayList<>();
		/**
		 * calculate distance of all delivery boys
		 */
		for (DeliveryBoy deliveryBoy : availableDeliveryBoys) {
			DeliveryBoyLocation deliveryBoyLocation = deliveryBoyLocationService.getDeliveryBoyLatestLocation(deliveryBoy.getId());
			Double distance = CommonUtility.distance(vendor.getLatitude().doubleValue(), vendor.getLongitude().doubleValue(),
					deliveryBoyLocation.getLatitude().doubleValue(), deliveryBoyLocation.getLongitude().doubleValue());

			deliveryBoyWithDistanceMap.put(deliveryBoy.getId(), distance);
		}

		Double firstMin = Double.MAX_VALUE;
		Double secMin = Double.MAX_VALUE;
		Double thirdMin = Double.MAX_VALUE;
		Long firstMinDeliveryBoyId = null;
		Long secMinDeliveryBoyId = null;
		Long thirdMinDeliveryBoyId = null;
		for (Entry<Long, Double> deliveryBoyWithDistanceEntrySet : deliveryBoyWithDistanceMap.entrySet()) {
			/**
			 * Check if delivery boy's distance is less than first min distance, then update
			 * first, second and third
			 */
			if (deliveryBoyWithDistanceEntrySet.getValue() < firstMin) {
				thirdMin = secMin;
				secMin = firstMin;
				firstMin = deliveryBoyWithDistanceEntrySet.getValue();
				thirdMinDeliveryBoyId = secMinDeliveryBoyId;
				secMinDeliveryBoyId = firstMinDeliveryBoyId;
				firstMinDeliveryBoyId = deliveryBoyWithDistanceEntrySet.getKey();
			}

			/**
			 * Check if delivery boy's distance is less than sec min distance then update
			 * second and third
			 */
			else if (deliveryBoyWithDistanceEntrySet.getValue() < secMin) {
				thirdMin = secMin;
				secMin = deliveryBoyWithDistanceEntrySet.getValue();
				thirdMinDeliveryBoyId = secMinDeliveryBoyId;
				secMinDeliveryBoyId = deliveryBoyWithDistanceEntrySet.getKey();
			}

			/**
			 * Check if delivery boy's distance is less than third min distance then update
			 * third
			 */
			else if (deliveryBoyWithDistanceEntrySet.getValue() < thirdMin) {
				thirdMin = deliveryBoyWithDistanceEntrySet.getValue();
				thirdMinDeliveryBoyId = deliveryBoyWithDistanceEntrySet.getKey();
			}
		}

		if (firstMinDeliveryBoyId != null) {
			nearestDeliveryBoys.add(firstMinDeliveryBoyId);
		}
		if (secMinDeliveryBoyId != null) {
			nearestDeliveryBoys.add(secMinDeliveryBoyId);
		}
		if (thirdMinDeliveryBoyId != null) {
			nearestDeliveryBoys.add(thirdMinDeliveryBoyId);
		}
		return nearestDeliveryBoys;
	}

	@Override
	public synchronized void acceptOrder(final Long deliveryBoyId, final Long orderId) throws NotFoundException, ValidationException {

		// check is order already accepted then throw exception else set delivery boy in
		// order
		/**
		 * remove order id from notification history table of delivery boy
		 *
		 */
		List<DeliveryBoySendNotificationHistory> deliveryBoySendNotificationHistoryList = deliveryBoySendNotificationHistoryRepository
				.findAllByOrderId(orderId);
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(deliveryBoySendNotificationHistoryList)) {
			for (DeliveryBoySendNotificationHistory deliveryBoyNotificationHistory : deliveryBoySendNotificationHistoryList) {
				deliveryBoyNotificationHistory.setOrderId(null);
			}
			deliveryBoySendNotificationHistoryRepository.saveAll(deliveryBoySendNotificationHistoryList);
		}
	}

	@Override
	public void validateBeforeLogout() throws NotFoundException, ValidationException {
		Long userId = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser().getId();
		Optional<UserLogin> userLogin = userLoginService.getUserLogin(userId);
		if (userLogin.isPresent() && UserType.DELIVERY_BOY.name().equals(userLogin.get().getEntityType())) {
			DeliveryBoy deliveryBoy = getDeliveryBoyDetail(userLogin.get().getEntityId());
			/**
			 * If assigned order exist then can't logged out
			 */
			if (deliveryBoy.getIsBusy().booleanValue()) {
				throw new ValidationException(messageByLocaleService.getMessage("logout.assigned.order.exist", null));
			} else {
				deliveryBoy.setIsLogin(false);
				deliveryBoyRepository.save(deliveryBoy);
			}
		}
	}

	@Override
	public void deliverOrder(final Long deliveryBoyId, final Long orderId) throws NotFoundException {
		// update order's status here
		DeliveryBoy deliveryBoy = getDeliveryBoyDetail(deliveryBoyId);
		/**
		 * set isBusy to false if delivery boy has no any other assigned orders
		 */
		deliveryBoy.setIsBusy(false);
		deliveryBoyRepository.save(deliveryBoy);
	}

	@Override
	public synchronized void updateDeliveryBoyRating(final Long deliveryBoyId, final Double ratingByClient) throws NotFoundException {
		DeliveryBoy deliveryBoy = getDeliveryBoyDetail(deliveryBoyId);
		Double updatedRating = ((deliveryBoy.getRating() * deliveryBoy.getNoOfRating()) + ratingByClient) / (deliveryBoy.getNoOfRating() + 1);
		deliveryBoy.setRating(updatedRating);
		deliveryBoy.setNoOfRating(deliveryBoy.getNoOfRating() + 1);
		deliveryBoyRepository.save(deliveryBoy);
	}

	@Override
	public Boolean isPhoneNumberExists(final DeliveryBoyDTO deliveryBoyDTO) {
		if (deliveryBoyDTO.getId() != null) {
			/**
			 * At the time of update is delivery boy with same phone number exist or not
			 * except it's own id
			 */
			return deliveryBoyRepository.findByPhoneNumberIgnoreCaseAndIdNot(deliveryBoyDTO.getPhoneNumber(), deliveryBoyDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is delivery boy with same contact exist or not
			 */
			return deliveryBoyRepository.findByPhoneNumberIgnoreCase(deliveryBoyDTO.getPhoneNumber()).isPresent();
		}
	}

	
}
