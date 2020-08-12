package com.nice.service.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
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
import com.nice.constant.Constant;
import com.nice.constant.DeliveryBoyStatus;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.OrderStatusEnum;
import com.nice.constant.Role;
import com.nice.constant.SendingType;
import com.nice.constant.TaskStatusEnum;
import com.nice.constant.TaskTypeEnum;
import com.nice.constant.UserOtpTypeEnum;
import com.nice.constant.UserType;
import com.nice.dto.AssignedOrdersCountDTO;
import com.nice.dto.DashBoardDetailDTO;
import com.nice.dto.DeliveryBoyAccountDetailsDTO;
import com.nice.dto.DeliveryBoyDTO;
import com.nice.dto.DeliveryBoyPersonalDetailsDTO;
import com.nice.dto.DeliveryBoyResponseDTO;
import com.nice.dto.Notification;
import com.nice.dto.OrderNotificationDTO;
import com.nice.dto.TaskDto;
import com.nice.dto.TaskFilterDTO;
import com.nice.dto.UserOtpDto;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.jms.queue.JMSQueuerService;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.DeliveryBoyMapper;
import com.nice.model.CashCollection;
import com.nice.model.DeliveryBoy;
import com.nice.model.DeliveryBoyCurrentStatus;
import com.nice.model.DeliveryBoyLocation;
import com.nice.model.DeliveryBoySendNotificationHistory;
import com.nice.model.Orders;
import com.nice.model.Task;
import com.nice.model.UserLogin;
import com.nice.model.UserOtp;
import com.nice.model.Vendor;
import com.nice.repository.DeliveryBoyCurrentStatusRepository;
import com.nice.repository.DeliveryBoyRepository;
import com.nice.repository.DeliveryBoySendNotificationHistoryRepository;
import com.nice.service.AssetService;
import com.nice.service.CashcollectionService;
import com.nice.service.DeliveryBoyLocationService;
import com.nice.service.DeliveryBoyService;
import com.nice.service.OrdersService;
import com.nice.service.OtpService;
import com.nice.service.TaskService;
import com.nice.service.UserLoginService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;
import com.nice.util.ExportCSV;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
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
	private DeliveryBoyCurrentStatusRepository deliveryBoyCurrentStatusRepository;

	@Autowired
	private AssetService assetService;

	@Autowired
	private OtpService otpService;

	@Autowired
	private JMSQueuerService jmsQueuerService;

	@Autowired
	private OrdersService ordersService;

	@Autowired
	private TaskService taskService;

	@Autowired
	private CashcollectionService cashCollectionService;

	@Override
	public void addDeliveryBoy(final DeliveryBoyDTO deliveryBoyDTO, final MultipartFile profilePicture) throws ValidationException, NotFoundException {
		DeliveryBoy deliveryBoy = deliveryBoyMapper.toEntity(deliveryBoyDTO);

		/**
		 * Check if delivery boy already exists, if so then lets only send him email again.
		 */
		Optional<DeliveryBoy> optDeliveryBoy = deliveryBoyRepository.findByEmail(deliveryBoyDTO.getEmail().toLowerCase());
		if (optDeliveryBoy.isPresent() && !optDeliveryBoy.get().getEmailVerified().booleanValue()) {
			deliveryBoy = optDeliveryBoy.get();
			Optional<UserLogin> optUserLogin = userLoginService.getUserLoginBasedOnEmailAndRole(deliveryBoyDTO.getEmail().toLowerCase(),
					Role.DELIVERY_BOY.name());
			if (optUserLogin.isPresent()) {
				sendOtpForEmailVerification(optUserLogin.get(), deliveryBoy);
				return;
			}
		}
		uploadImage(profilePicture, deliveryBoy);
		deliveryBoy.setEmailVerified(false);
		deliveryBoy.setPhoneVerified(false);
		deliveryBoy.setActive(false);
		deliveryBoy.setStatus(DeliveryBoyStatus.PENDING.getStatusValue());
		deliveryBoy.setNoOfRating(0L);
		deliveryBoy.setRating(0D);
		deliveryBoy = deliveryBoyRepository.save(deliveryBoy);

		/**
		 * set delivery boy's current status
		 */
		DeliveryBoyCurrentStatus deliveryBoyCurrentStatus = new DeliveryBoyCurrentStatus();
		deliveryBoyCurrentStatus.setDeliveryBoy(deliveryBoy);
		/**
		 * it will be true when he will logged in
		 */
		deliveryBoyCurrentStatus.setIsLogin(false);
		/**
		 * it will be true when he is going for delivery
		 */
		deliveryBoyCurrentStatus.setIsBusy(false);
		/**
		 * it will be true when he is able to deliver order(getting notifications for delivery)
		 */
		deliveryBoyCurrentStatus.setIsAvailable(false);
		deliveryBoyCurrentStatus.setActive(true);
		deliveryBoyCurrentStatusRepository.save(deliveryBoyCurrentStatus);

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
	public void updatePersonalDetails(final DeliveryBoyPersonalDetailsDTO deliveryBoyPersonalDetailsDTO) throws NotFoundException, ValidationException {
		DeliveryBoy deliveryBoy = getDeliveryBoyDetail(deliveryBoyPersonalDetailsDTO.getId());
		BeanUtils.copyProperties(deliveryBoyPersonalDetailsDTO, deliveryBoy);
		deliveryBoyRepository.save(deliveryBoy);
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
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("firstName"));
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
	public DeliveryBoyCurrentStatus getDeliveryBoyCurrentStatusDetail(final DeliveryBoy deliveryBoy) throws NotFoundException {
		return deliveryBoyCurrentStatusRepository.findByDeliveryBoy(deliveryBoy).orElseThrow(
				() -> new NotFoundException(messageByLocaleService.getMessage("deliveryboy.current.status.not.found", new Object[] { deliveryBoy.getId() })));
	}

	@Override
	public void exportList(final Boolean activeRecords, final String searchKeyword, final HttpServletResponse httpServletResponse)
			throws FileNotFoundException {
		List<DeliveryBoy> deliveryBoyList;
		List<DeliveryBoyResponseDTO> deliveryBoyDtoList = new ArrayList<>();
		if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(searchKeyword)) {
			if (activeRecords != null) {
				deliveryBoyList = deliveryBoyRepository.findAllByActiveAndFirstNameContainingIgnoreCaseOrLastNameContainingIgnoreCase(activeRecords,
						searchKeyword, searchKeyword);
			} else {
				deliveryBoyList = deliveryBoyRepository.findAllByFirstNameContainingIgnoreCaseOrLastNameContainingIgnoreCase(searchKeyword, searchKeyword);
			}
		} else {
			if (activeRecords != null) {
				deliveryBoyList = deliveryBoyRepository.findAllByActive(activeRecords);
			} else {
				deliveryBoyList = deliveryBoyRepository.findAll();
			}
		}
		for (DeliveryBoy deliveryBoy : deliveryBoyList) {
			deliveryBoyDtoList.add(deliveryBoyMapper.toDto(deliveryBoy));
		}
		final Object[] deliveryBoyHeaderField = new Object[] { "Delivery Boy Name", "Email", "Gender", "Phone Number", "Bank Name", "Branch Name",
				"Acount Name", "Bank Account Number", "Kib No", "Branch City" };
		final Object[] deliveryBoyDataField = new Object[] { "name", "email", "gender", "phoneNumber", "bankName", "branchName", "accountName",
				"bankAccountNumber", "kibNo", "branchCity" };
		try {
			exportCSV.writeCSVFile(deliveryBoyDtoList, deliveryBoyDataField, deliveryBoyHeaderField, httpServletResponse);
		} catch (IOException e) {
			throw new FileNotFoundException(messageByLocaleService.getMessage("export.file.create.error", null));
		}
	}

	@Override
	public String changeStatus(final Long deliveryBoyId, final Boolean active) throws NotFoundException, ValidationException {
		DeliveryBoy deliveryBoy = getDeliveryBoyDetail(deliveryBoyId);
		DeliveryBoyCurrentStatus deliveryBoyCurrentStatus = getDeliveryBoyCurrentStatusDetail(deliveryBoy);
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
				if (deliveryBoyCurrentStatus.getIsBusy().booleanValue()) {
					throw new ValidationException(messageByLocaleService.getMessage("deactive.assigned.order.exist", null));
				}
				deliveryBoy.setStatus(DeliveryBoyStatus.DE_ACTIVE.getStatusValue());
				deliveryBoyCurrentStatus.setIsLogin(false);
			} else {
				if (!deliveryBoy.getEmailVerified().booleanValue()) {
					throw new ValidationException(messageByLocaleService.getMessage("email.not.verified", null));
				}
				deliveryBoy.setStatus(DeliveryBoyStatus.ACTIVE.getStatusValue());
			}
			userLogin.get().setActive(active);
			userLoginService.updateUserLogin(userLogin.get());
			deliveryBoy.setActive(active);
			deliveryBoy = deliveryBoyRepository.save(deliveryBoy);
			deliveryBoyCurrentStatus.setDeliveryBoy(deliveryBoy);
			deliveryBoyCurrentStatusRepository.save(deliveryBoyCurrentStatus);
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
			 * At the time of update is deliveryBoy with same email exist or not except it's own id
			 */
			return deliveryBoyRepository.findByEmailAndIdNot(deliveryBoyDTO.getEmail().toLowerCase(), deliveryBoyDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is deliveryBoy with same email exist or not
			 */
			Optional<DeliveryBoy> optDeliveryboy = deliveryBoyRepository.findByEmail(deliveryBoyDTO.getEmail().toLowerCase());
			if (optDeliveryboy.isPresent()) {
				/**
				 * If the delivery boy is present and his email not verified, then we will be sending the verification link for him
				 * again, if the email is verified then we will be returning true.
				 */

				return optDeliveryboy.get().getEmailVerified();
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
		deliveryBoy.setEmailVerified(true);
		deliveryBoy.setStatus(DeliveryBoyStatus.ACTIVE.getStatusValue());
		deliveryBoyRepository.save(deliveryBoy);
	}

	@Override
	public void updateIsLogin(final String userName) throws NotFoundException {
		Optional<UserLogin> optUserLogin = userLoginService.getUserLoginBasedOnEmailAndRole(userName, Role.DELIVERY_BOY.name());
		if (optUserLogin.isPresent()) {
			DeliveryBoyCurrentStatus deliveryBoyCurrentStatus = getDeliveryBoyCurrentStatusDetail(getDeliveryBoyDetail(optUserLogin.get().getEntityId()));
			deliveryBoyCurrentStatus.setIsLogin(true);
			deliveryBoyCurrentStatusRepository.save(deliveryBoyCurrentStatus);
		} else {
			throw new NotFoundException(messageByLocaleService.getMessage("user.not.exists.email", new Object[] { userName }));
		}
	}

	@Override
	public void updateIsAvailable(final Boolean isAvailable) throws NotFoundException, ValidationException {
		/**
		 * update delivery boy is available for delivering orders
		 */
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (UserType.DELIVERY_BOY.name().equals(userLogin.getEntityType())) {
			DeliveryBoyCurrentStatus deliveryBoyCurrentStatus = getDeliveryBoyCurrentStatusDetail(getDeliveryBoyDetail(userLogin.getEntityId()));
			deliveryBoyCurrentStatus.setIsAvailable(isAvailable);
			deliveryBoyCurrentStatusRepository.save(deliveryBoyCurrentStatus);
			LOGGER.info("update is available for delivery boy :{} and isAvailable:{}", userLogin.getEntityId(), isAvailable);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
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
		userOtpDto.setUserId(userLogin.getId());
		UserOtp otp = otpService.generateOtp(userOtpDto);

		sendEmail(otp.getOtp(), userLogin.getId(), deliveryBoy.getEmail());
	}

	private void sendEmail(final String otp, final Long userId, final String email) {
		Notification notification = new Notification();
		notification.setOtp(otp);
		notification.setUserId(userId);
		notification.setEmail(email);
		notification.setUserType(UserType.DELIVERY_BOY.name());
		notification.setSendingType(SendingType.OTP.name());
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
			assetService.deleteFile(deliveryBoy.getProfilePictureName(), AssetConstant.DELIVERY_BOY);
		}
	}

	@Override
	public List<Long> getNextThreeNearestDeliveryBoysFromVendor(final Long orderId, final Long vendorId) throws NotFoundException {
		Vendor vendor = vendorService.getVendorDetail(vendorId);
		/**
		 * get all delivery boys who is logged in, not busy with any orders and has not sended notification before
		 */
		List<DeliveryBoy> availableDeliveryBoys = deliveryBoyRepository.getAllNextAvailableDeliveryBoys(orderId);
		List<DeliveryBoy> busyDeliveryBoys = new ArrayList<>();
		/**
		 * if idle delivery boys is not available then go for a busy delivery boys who is going for delivery of orders(not for
		 * replacement or return) and at a time assigned order count is 1
		 */
		if (availableDeliveryBoys.isEmpty()) {
			busyDeliveryBoys = deliveryBoyRepository.getAllNextAvailableDeliveryBoysOnBusyTime(orderId);
		}
		/**
		 * remove all busy delivery boys who has more then one assigned orders
		 */
		List<DeliveryBoy> removeDeliveryBoys = new ArrayList<>();
		for (DeliveryBoy deliveryBoy : busyDeliveryBoys) {
			TaskFilterDTO taskFilterDTO = new TaskFilterDTO();
			taskFilterDTO.setDeliveryBoyId(deliveryBoy.getId());
			taskFilterDTO.setStatusListNotIn(Arrays.asList(TaskStatusEnum.DELIVERED.getStatusValue()));
			Long count = taskService.getTaskCountBasedOnParams(taskFilterDTO);
			if (count > 1) {
				removeDeliveryBoys.add(deliveryBoy);
			}
		}
		busyDeliveryBoys.removeAll(removeDeliveryBoys);
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
			 * Check if delivery boy's distance is less than first min distance, then update first, second and third
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
			 * Check if delivery boy's distance is less than sec min distance then update second and third
			 */
			else if (deliveryBoyWithDistanceEntrySet.getValue() < secMin) {
				thirdMin = secMin;
				secMin = deliveryBoyWithDistanceEntrySet.getValue();
				thirdMinDeliveryBoyId = secMinDeliveryBoyId;
				secMinDeliveryBoyId = deliveryBoyWithDistanceEntrySet.getKey();
			}

			/**
			 * Check if delivery boy's distance is less than third min distance then update third
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
		/**
		 * check is order already accepted then throw exception else set delivery boy in order
		 */
		Orders orders = ordersService.getOrderById(orderId);
		if (!OrderStatusEnum.CONFIRMED.getStatusValue().equals(orders.getOrderStatus())) {
			throw new ValidationException(messageByLocaleService.getMessage("order.already.accepted", null));
		}
		/**
		 * update delivery boy's current status to is busy
		 */
		DeliveryBoy deliveryBoy = getDeliveryBoyDetail(deliveryBoyId);
		DeliveryBoyCurrentStatus deliveryBoyCurrentStatus = getDeliveryBoyCurrentStatusDetail(deliveryBoy);
		deliveryBoyCurrentStatus.setIsBusy(true);
		deliveryBoyCurrentStatusRepository.save(deliveryBoyCurrentStatus);
		/**
		 * change order status
		 */
		orders.setDeliveryBoy(deliveryBoy);
		ordersService.changeStatus(Constant.IN_PROCESS, orders);

		/**
		 * create a delivery task for delivery boy
		 */
		TaskDto taskDto = new TaskDto();
		taskDto.setDeliveryBoyId(deliveryBoyId);
		taskDto.setOrderId(orders.getId());
		taskDto.setTaskType(TaskTypeEnum.DELIVERY.getTaskValue());

		taskService.createTask(taskDto);

		/**
		 * remove delivery boy notification history for this order
		 */
		List<DeliveryBoySendNotificationHistory> deliveryBoySendNotificationHistoryList = deliveryBoySendNotificationHistoryRepository
				.findAllByOrderId(orderId);
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(deliveryBoySendNotificationHistoryList)) {
			deliveryBoySendNotificationHistoryRepository.deleteAll(deliveryBoySendNotificationHistoryList);
		}
	}

	@Override
	public void validateBeforeLogout() throws NotFoundException, ValidationException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (UserType.DELIVERY_BOY.name().equals(userLogin.getEntityType())) {
			DeliveryBoyCurrentStatus deliveryBoyCurrentStatus = getDeliveryBoyCurrentStatusDetail(getDeliveryBoyDetail(userLogin.getEntityId()));
			/**
			 * If assigned order exist then can't logged out
			 */
			if (deliveryBoyCurrentStatus.getIsBusy().booleanValue()) {
				throw new ValidationException(messageByLocaleService.getMessage("logout.assigned.order.exist", null));
			} else {
				deliveryBoyCurrentStatus.setIsLogin(false);
				deliveryBoyCurrentStatusRepository.save(deliveryBoyCurrentStatus);
			}
		}
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
			 * At the time of update is delivery boy with same phone number exist or not except it's own id
			 */
			return deliveryBoyRepository.findByPhoneNumberIgnoreCaseAndIdNot(deliveryBoyDTO.getPhoneNumber(), deliveryBoyDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is delivery boy with same phone number exist or not
			 */
			Optional<DeliveryBoy> optDeliveryBoy = deliveryBoyRepository.findByPhoneNumberIgnoreCase(deliveryBoyDTO.getPhoneNumber());
			if (optDeliveryBoy.isPresent()) {
				return !deliveryBoyDTO.getEmail().equalsIgnoreCase(optDeliveryBoy.get().getEmail());
			} else {
				return false;
			}
		}
	}

	@Override
	public OrderNotificationDTO getOrderDetailInDeliveryBoyAcceptNotification(final Long orderId, final Long deliveryBoyId) throws NotFoundException {
		Double distance = null;
		String pickUpAddress = null;
		String dropAddress = null;

		Orders orders = ordersService.getOrderById(orderId);
		OrderNotificationDTO orderNotificationDTO = new OrderNotificationDTO();
		BeanUtils.copyProperties(orders, orderNotificationDTO);
		DeliveryBoyLocation deliveryBoyLocation = deliveryBoyLocationService.getDeliveryBoyLatestLocation(deliveryBoyId);
		if (OrderStatusEnum.CONFIRMED.getStatusValue().equalsIgnoreCase(orders.getOrderStatus())
				|| OrderStatusEnum.IN_PROCESS.getStatusValue().equalsIgnoreCase(orders.getOrderStatus())) {
			pickUpAddress = getVendorAddress(orders.getVendor());
			dropAddress = orders.getAddress();
			/**
			 * distance will be delivery boy's distance from vendor + distance between vendor and delivery location
			 */
			distance = CommonUtility.distance(deliveryBoyLocation.getLatitude().doubleValue(), deliveryBoyLocation.getLongitude().doubleValue(),
					orders.getVendor().getLatitude().doubleValue(), orders.getVendor().getLongitude().doubleValue())
					+ CommonUtility.distance(orders.getLatitude().doubleValue(), orders.getLongitude().doubleValue(),
							orders.getVendor().getLatitude().doubleValue(), orders.getVendor().getLongitude().doubleValue());
			orderNotificationDTO.setDistance(distance);
		} else if (OrderStatusEnum.REPLACE_PROCESSED.getStatusValue().equalsIgnoreCase(orders.getOrderStatus())) {
			/**
			 * at the time of replacement delivery boy goes to customer's location first
			 */
			pickUpAddress = orders.getAddress();
			dropAddress = getVendorAddress(orders.getVendor());
			/**
			 * distance will be delivery boy's distance from customer + 2 times distance between vendor and customer
			 */
			distance = CommonUtility.distance(deliveryBoyLocation.getLatitude().doubleValue(), deliveryBoyLocation.getLongitude().doubleValue(),
					orders.getLatitude().doubleValue(), orders.getLongitude().doubleValue())
					+ 2 * CommonUtility.distance(orders.getLatitude().doubleValue(), orders.getLongitude().doubleValue(),
							orders.getVendor().getLatitude().doubleValue(), orders.getVendor().getLongitude().doubleValue());

		}
		orderNotificationDTO.setPickUpAddress(pickUpAddress);
		orderNotificationDTO.setDropAddress(dropAddress);
		orderNotificationDTO.setDistance(distance);
		return orderNotificationDTO;
	}

	protected String getVendorAddress(final Vendor vendor) {
		StringBuilder address = new StringBuilder();
		address.append(vendor.getBlock()).append(",").append(vendor.getBuilding()).append(",").append(vendor.getStreet()).append(",").append(vendor.getArea())
				.append(",").append(vendor.getCity().getName()).append(",").append(vendor.getPincode().getCodeValue());
		return address.toString();
	}

	@Override
	public AssignedOrdersCountDTO getAssignedOrdersCount(final Long deliveryBoyId) throws NotFoundException, ValidationException {
		AssignedOrdersCountDTO assignedOrdersCountDTO = new AssignedOrdersCountDTO();
		Map<String, Integer> assignedOrdersCountMap = new HashMap<>();
		/**
		 * regular orders
		 */
		TaskFilterDTO taskFilterDTO = new TaskFilterDTO();
		taskFilterDTO.setDeliveryBoyId(deliveryBoyId);
		taskFilterDTO.setTaskType(TaskTypeEnum.DELIVERY.getTaskValue());
		taskFilterDTO.setStatusListNotIn(Arrays.asList(TaskStatusEnum.DELIVERED.getStatusValue()));
		Long regularOrders = taskService.getTaskCountBasedOnParams(taskFilterDTO);
		assignedOrdersCountMap.put("Regular Orders", regularOrders.intValue());
		assignedOrdersCountDTO.setDeliveryBoyId(deliveryBoyId);
		assignedOrdersCountDTO.setAssignedOrdersCountMap(assignedOrdersCountMap);
		// return replace remaininig
		return assignedOrdersCountDTO;
	}

	@Override
	public DashBoardDetailDTO getDashBoard(final Long deliveryBoyId) throws NotFoundException, ValidationException {
		DeliveryBoy deliveryBoy = getDeliveryBoyDetail(deliveryBoyId);
		DeliveryBoyCurrentStatus deliveryBoyCurrentStatus = getDeliveryBoyCurrentStatusDetail(deliveryBoy);
		DashBoardDetailDTO dashBoardDetailDTO = new DashBoardDetailDTO();
		dashBoardDetailDTO.setDeliveryBoyId(deliveryBoyId);
		/**
		 * delivery boy is active(available) for taking orders
		 */
		dashBoardDetailDTO.setIsAvailable(deliveryBoyCurrentStatus.getIsAvailable());
		/**
		 * assigned orders count
		 */
		TaskFilterDTO taskFilterDTO = new TaskFilterDTO();
		taskFilterDTO.setDeliveryBoyId(deliveryBoyId);
		taskFilterDTO.setStatusListNotIn(Arrays.asList(TaskStatusEnum.DELIVERED.getStatusValue()));
		Long count = taskService.getTaskCountBasedOnParams(taskFilterDTO);
		dashBoardDetailDTO.setAssignedOrdersCount(count.intValue());
		/**
		 * today's delivered orders count
		 */
		taskFilterDTO.setStatusListNotIn(null);
		taskFilterDTO.setTaskType(null);
		taskFilterDTO.setStatusList(Arrays.asList(TaskStatusEnum.DELIVERED.getStatusValue()));
		taskFilterDTO.setDeliveredDate(new Date(System.currentTimeMillis()));
		count = taskService.getTaskCountBasedOnParams(taskFilterDTO);
		dashBoardDetailDTO.setDeliveredOrdersCount(count.intValue());
		/**
		 * for on going order
		 */
		taskFilterDTO.setStatusList(Arrays.asList(TaskStatusEnum.PICK_UP_ON_WAY.getStatusValue(), TaskStatusEnum.REACHED_VENDOR.getStatusValue(),
				TaskStatusEnum.ON_THE_WAY.getStatusValue()));
		taskFilterDTO.setDeliveredDate(null);
		List<Task> taskList = taskService.getTaskListBasedOnParams(taskFilterDTO, null, null);
		if (taskList.size() > 1) {
			throw new ValidationException("morethen.one.ongoing.order", null);
		}
		for (Task task : taskList) {
			dashBoardDetailDTO.setOnGoingOrderId(task.getOrder().getId());
		}
		/**
		 * for today's total cash collection
		 */
		Double totalCash = 0d;
		List<CashCollection> cashCollectionList = cashCollectionService.getListBasedOnParams(null, null, deliveryBoyId, new Date(System.currentTimeMillis()));
		for (CashCollection cashCollection : cashCollectionList) {
			totalCash += cashCollection.getAmount();
		}
		dashBoardDetailDTO.setTodaysCashCollection(totalCash);
		return dashBoardDetailDTO;
	}

}
