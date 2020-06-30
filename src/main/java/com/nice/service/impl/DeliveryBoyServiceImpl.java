package com.nice.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

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
import com.nice.model.UserLogin;
import com.nice.model.UserOtp;
import com.nice.model.Vendor;
import com.nice.repository.DeliveryBoyRepository;
import com.nice.service.AssetService;
import com.nice.service.DeliveryBoyLocationService;
import com.nice.service.DeliveryBoyService;
import com.nice.service.FileStorageService;
import com.nice.service.OtpService;
import com.nice.service.UserLoginService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;

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
	private DeliveryBoyRepository deliveryBoyRepository;

	@Autowired
	private DeliveryBoyMapper deliveryBoyMapper;

	@Autowired
	private DeliveryBoyLocationService deliveryBoyLocationService;

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
		if (!deliveryBoy.getEmail().equals(deliveryBoyPersonalDetailsDTO.getEmail())) {
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

				final Optional<UserLogin> userLogin = userLoginService.getUserLoginBasedOnEmailAndRole(deliveryBoy.getEmail(), Role.DELIVERY_BOY.name());
				if (userLogin.isPresent()) {
					userLogin.get().setEmail(deliveryBoyPersonalDetailsDTO.getEmail());
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
	public Page<DeliveryBoy> getDeliveryBoyList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final Boolean isEmailVerified)
			throws NotFoundException {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("id"));
		if (isEmailVerified != null) {
			if (activeRecords != null) {
				return deliveryBoyRepository.findAllByActiveAndIsEmailVerified(activeRecords, isEmailVerified, pageable);
			} else {
				return deliveryBoyRepository.findAllByIsEmailVerified(isEmailVerified, pageable);
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
			return deliveryBoyRepository.findByEmailAndIdNot(deliveryBoyDTO.getEmail(), deliveryBoyDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is deliveryBoy with same email exist or not
			 */
			return deliveryBoyRepository.findByEmail(deliveryBoyDTO.getEmail()).isPresent();
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
	public Boolean isUserLoginExists(final DeliveryBoyDTO deliveryBoyDTO) {
		Optional<UserLogin> optUserLogin = userLoginService.getUserLoginBasedOnEmailAndRole(deliveryBoyDTO.getEmail(), Role.DELIVERY_BOY.name());
		if (optUserLogin.isPresent()) {
			if (deliveryBoyDTO.getId() != null && deliveryBoyDTO.getId().equals(optUserLogin.get().getEntityId())) {
				return false;
			} else {
				return true;
			}
		} else {
			return false;
		}
	}

	@Override
	public void verifyEmail(final Long deliveryBoyId) throws NotFoundException {
		DeliveryBoy deliveryBoy = getDeliveryBoyDetail(deliveryBoyId);
		deliveryBoy.setIsEmailVerified(true);
		deliveryBoyRepository.save(deliveryBoy);
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

	public List<Long> getNextThreeNearestDeliveryBoysFromVendor(final Long vendorId, final List<Long> acceptDeliveryBoyIds) throws NotFoundException {
		Vendor vendor = vendorService.getVendorDetail(vendorId);
		/**
		 * get all delivery boys who is logged in, not busy with any orders and has not
		 * sended notification before
		 */
		List<DeliveryBoy> availableDeliveryBoys = deliveryBoyRepository.findAllByIsLoginAndIsBusyAndIdNotIn(true, false, acceptDeliveryBoyIds);
		List<DeliveryBoy> busyDeliveryBoys = new ArrayList<>();
		/**
		 * if idle delivery boys is less then three then go for a busy delivery boys who
		 * is going for delivery of orders(not for replacement or return)
		 */
		if (availableDeliveryBoys.size() < 3) {
			busyDeliveryBoys = deliveryBoyRepository.findAllByIsLoginAndIsBusyAndIdNotIn(true, true, acceptDeliveryBoyIds);
		}
		availableDeliveryBoys.addAll(busyDeliveryBoys);
		Map<Long, Double> deliveryBoyWithDistanceMap = new HashMap<>();
		List<Long> nearestDeliveryBoys = new ArrayList<>();
		/**
		 * calculate distance of all delivery boys
		 */
		for (DeliveryBoy deliveryBoy : availableDeliveryBoys) {
			DeliveryBoyLocation deliveryBoyLocation = deliveryBoyLocationService.getDeliveryBoyLatestLocation(deliveryBoy.getId());
			Double distance;
			if (deliveryBoy.getIsBusy().booleanValue()) {
				/**
				 * if delivery boy is out for delivery then total distance will be (his distance
				 * from customer + distance between customer and vandor's location)
				 */
				// get customer's latitude and longitude here

				Double customerLatidude = null;
				Double customerLongitude = null;
				distance = CommonUtility.distance(customerLatidude, customerLongitude, deliveryBoyLocation.getLatitude().doubleValue(),
						deliveryBoyLocation.getLongitude().doubleValue())
						+ CommonUtility.distance(vendor.getLatitude().doubleValue(), vendor.getLongitude().doubleValue(), customerLatidude, customerLongitude);
			} else {
				distance = CommonUtility.distance(vendor.getLatitude().doubleValue(), vendor.getLongitude().doubleValue(),
						deliveryBoyLocation.getLatitude().doubleValue(), deliveryBoyLocation.getLongitude().doubleValue());
			}
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
	public void sendNotificationToDeliveryBoysForAcceptingOrder(final Long orderId, final Long vendorId)
			throws NotFoundException, InterruptedException, ValidationException {
		/**
		 * send notification to only those delivery boys for which we have not sended
		 * previously
		 */
		List<Long> acceptDeliveryBoyIds = new ArrayList<>();
		List<Long> nextNearestDeliveryBoys = getNextThreeNearestDeliveryBoysFromVendor(vendorId, acceptDeliveryBoyIds);
		/**
		 * if not a single delivery boy is logged in for accepting order then throw
		 * exception
		 */
		if (!CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(nextNearestDeliveryBoys)) {
			throw new ValidationException(messageByLocaleService.getMessage("deliveryboy.not.available", null));
		} else {
			sendNotificationForNewOrder(nextNearestDeliveryBoys, orderId);
			acceptDeliveryBoyIds.addAll(nextNearestDeliveryBoys);
			TimeUnit.SECONDS.sleep(30);
			// check if order is accepted then do not send notification
			nextNearestDeliveryBoys = getNextThreeNearestDeliveryBoysFromVendor(vendorId, acceptDeliveryBoyIds);
			if (!CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(nextNearestDeliveryBoys)) {
				sendNotificationForNewOrder(nextNearestDeliveryBoys, orderId);
				acceptDeliveryBoyIds.addAll(nextNearestDeliveryBoys);
				TimeUnit.SECONDS.sleep(30);
			}
			// check if order is accepted then do not send notification
			nextNearestDeliveryBoys = getNextThreeNearestDeliveryBoysFromVendor(vendorId, acceptDeliveryBoyIds);
			if (!CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(nextNearestDeliveryBoys)) {
				sendNotificationForNewOrder(nextNearestDeliveryBoys, orderId);
			}
		}

	}

	/**
	 * @param nextNearestDeliveryBoys
	 */
	private void sendNotificationForNewOrder(final List<Long> nextNearestDeliveryBoys, final Long orderId) {
		// PushNotification pushNotification = new PushNotification();
		// pushNotification.setDeliveryBoyIds(nextNearestDeliveryBoys);
		// pushNotification.setOrderId(orderId);
		// jmsQueuerService.sendPushNotification(NotificationQueueConstants.PUSH_NOTIFICATION_QUEUE_ACCEPT_ORDER,
		// pushNotification);
	}

	@Override
	public synchronized void acceptOrder(final Long deliveryBoyId, final Long orderId) throws NotFoundException, ValidationException {
		DeliveryBoy deliveryBoy = getDeliveryBoyDetail(deliveryBoyId);
		if (deliveryBoy.getIsBusy().booleanValue()) {
			throw new ValidationException(messageByLocaleService.getMessage("deliver.order.first", null));
		}
		// check is order already accepted then throw exception else set delivery boy in
		// order
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

}
