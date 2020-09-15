package com.nice.service.impl;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;

import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
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
import com.nice.dto.DashBoardDetailDTO;
import com.nice.dto.DeliveryBoyAccountDetailsDTO;
import com.nice.dto.DeliveryBoyDTO;
import com.nice.dto.DeliveryBoyFilterDTO;
import com.nice.dto.DeliveryBoyPersonalDetailsDTO;
import com.nice.dto.DeliveryBoyResponseDTO;
import com.nice.dto.Notification;
import com.nice.dto.OrderItemResponseDTO;
import com.nice.dto.OrdersCountDTO;
import com.nice.dto.OrdersDetailDTOForDeliveryBoy;
import com.nice.dto.OrdersListDTOForDeliveryBoy;
import com.nice.dto.TaskDto;
import com.nice.dto.TaskFilterDTO;
import com.nice.dto.UserOtpDto;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.FileOperationException;
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
import com.nice.model.DeviceDetail;
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
import com.nice.service.DeviceDetailService;
import com.nice.service.OrderItemService;
import com.nice.service.OrdersService;
import com.nice.service.OtpService;
import com.nice.service.RoleService;
import com.nice.service.TaskService;
import com.nice.service.UserLoginService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;
import com.nice.util.ExportCSV;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
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

	@Autowired
	private DeviceDetailService deviceDetailService;

	@Autowired
	private OrderItemService orderItemService;

	@Autowired
	private RoleService roleService;

	@Override
	public void addDeliveryBoy(final DeliveryBoyDTO deliveryBoyDTO, final MultipartFile profilePicture)
			throws ValidationException, NotFoundException, FileOperationException {
		DeliveryBoy deliveryBoy = deliveryBoyMapper.toEntity(deliveryBoyDTO);

		/**
		 * Check if delivery boy already exists, if so then lets only send him email again.
		 */
		Optional<DeliveryBoy> optDeliveryBoy = deliveryBoyRepository.findByEmail(deliveryBoyDTO.getEmail().toLowerCase());
		if (optDeliveryBoy.isPresent() && !optDeliveryBoy.get().getEmailVerified().booleanValue()) {
			deliveryBoy = optDeliveryBoy.get();
			Optional<UserLogin> optUserLogin = userLoginService.getUserLoginBasedOnEmailAndEntityType(deliveryBoyDTO.getEmail().toLowerCase(),
					UserType.DELIVERY_BOY.name());
			if (optUserLogin.isPresent()) {
				sendOtpForEmailVerification(optUserLogin.get(), deliveryBoy);
				return;
			}
		}
		/**
		 * Set delivery boy preferred language to default language when delivery boy registers.
		 */
		deliveryBoy.setPreferredLanguage(Constant.DEFAULT_LANGUAGE);

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
		userLogin.setRole(roleService.getRoleDetailByName(Role.DELIVERY_BOY.getStatusValue()));
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

	/**
	 * @param  sortByDirection
	 * @param  sortByField
	 * @return
	 * @throws ValidationException
	 */
	private void sortByFieldAndDirection(final DeliveryBoyFilterDTO deliveryBoyFilterDTO) throws ValidationException {
		validationForSortByFieldAndDirection(deliveryBoyFilterDTO);
		/**
		 * Default Field is id
		 */
		if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(deliveryBoyFilterDTO.getSortByField())) {
			if (!CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(deliveryBoyFilterDTO.getSortByDirection())) {
				deliveryBoyFilterDTO.setSortByDirection(Constant.SORT_DIRECTION_ASC);
			}
		} else {
			if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(deliveryBoyFilterDTO.getSortByDirection())) {
				deliveryBoyFilterDTO.setSortByField("id");
			} else {
				deliveryBoyFilterDTO.setSortByDirection(Constant.SORT_DIRECTION_DESC);
				deliveryBoyFilterDTO.setSortByField("id");
			}
		}
	}

	/**
	 *
	 * @param  sortByDirection
	 * @param  sortByField
	 * @throws ValidationException
	 */
	private void validationForSortByFieldAndDirection(final DeliveryBoyFilterDTO deliveryBoyFilterDTO) throws ValidationException {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(deliveryBoyFilterDTO.getSortByField())) {
			/**
			 * Validate sortByField is valid field or not using reflection
			 */
			Class<Vendor> vendorClass = Vendor.class;
			Field[] fields = vendorClass.getDeclaredFields();
			boolean isValid = false;
			for (Field field : fields) {
				if (deliveryBoyFilterDTO.getSortByField().equals(field.getName())) {
					isValid = true;
					break;
				}
			}
			if (!isValid) {
				throw new ValidationException(messageByLocaleService.getMessage("sort.field.invalid", null));
			}
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(deliveryBoyFilterDTO.getSortByDirection())
				&& !(Constant.SORT_DIRECTION_ASC.equals(deliveryBoyFilterDTO.getSortByDirection())
						|| Constant.SORT_DIRECTION_DESC.equals(deliveryBoyFilterDTO.getSortByDirection()))) {
			throw new ValidationException(messageByLocaleService.getMessage("sort.direction.invalid", null));
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
				deliveryBoyList = deliveryBoyRepository
						.findAllByActiveAndFirstNameEnglishContainingIgnoreCaseOrLastNameEnglishContainingIgnoreCaseOrFirstNameArabicContainingIgnoreCaseOrLastNameArabicContainingIgnoreCase(
								activeRecords, searchKeyword, searchKeyword, searchKeyword, searchKeyword);
			} else {
				deliveryBoyList = deliveryBoyRepository
						.findAllByFirstNameEnglishContainingIgnoreCaseOrLastNameEnglishContainingIgnoreCaseOrFirstNameArabicContainingIgnoreCaseOrLastNameArabicContainingIgnoreCase(
								searchKeyword, searchKeyword, searchKeyword, searchKeyword);
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
		final Optional<UserLogin> userLogin = userLoginService.getUserLoginBasedOnEmailAndEntityType(deliveryBoy.getEmail(), UserType.DELIVERY_BOY.name());
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
	public void updateProfilePicture(final MultipartFile profilePicture, final Long deliveryBoyId)
			throws NotFoundException, ValidationException, FileOperationException {
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
				 * If the delivery boy is present and his email not verified, then we will be sending the verification link for him again, if the email is
				 * verified then we will be returning true.
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
	 * @param  profilePicture
	 * @param  deliveryBoy
	 * @throws ValidationException
	 * @throws FileOperationException
	 */
	private void uploadImage(final MultipartFile profilePicture, final DeliveryBoy deliveryBoy) throws FileOperationException, ValidationException {
		deliveryBoy.setProfilePictureName(assetService.saveAsset(profilePicture, AssetConstant.DELIVERY_BOY, 0, 0, 0));
		deliveryBoy.setProfilePictureOriginalName(profilePicture.getOriginalFilename());
	}

	@Override
	public void verifyEmail(final Long deliveryBoyId) throws NotFoundException {
		DeliveryBoy deliveryBoy = getDeliveryBoyDetail(deliveryBoyId);
		deliveryBoy.setEmailVerified(true);
		deliveryBoy.setStatus(DeliveryBoyStatus.VERIFIED.getStatusValue());
		deliveryBoyRepository.save(deliveryBoy);
	}

	@Override
	public void updateIsLogin(final String userName) throws NotFoundException {
		Optional<UserLogin> optUserLogin = userLoginService.getUserLoginBasedOnEmailAndEntityType(userName, UserType.DELIVERY_BOY.name());
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
			if (Boolean.TRUE.equals(isAvailable)) {
				/**
				 * can not active if delivery boy location is not present
				 */
				List<DeliveryBoyLocation> deliveryBoyLocation = deliveryBoyLocationService.getDeliveryBoyLocationList(userLogin.getEntityId(), true);
				if (deliveryBoyLocation.isEmpty()) {
					throw new ValidationException(messageByLocaleService.getMessage("deliveryboy.location.required.active", null));
				}
				/**
				 * if delivery boy's device detail is not present then can not be available for accept order
				 */
				List<DeviceDetail> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLogin.getId());
				if (deviceDetailList.isEmpty()) {
					throw new ValidationException(messageByLocaleService.getMessage("deliveryboy.device.detail.required.active", null));
				}
			}
			DeliveryBoyCurrentStatus deliveryBoyCurrentStatus = getDeliveryBoyCurrentStatusDetail(getDeliveryBoyDetail(userLogin.getEntityId()));
			if (deliveryBoyCurrentStatus.getIsAvailable().equals(isAvailable)) {
				if (isAvailable.booleanValue()) {
					throw new ValidationException(messageByLocaleService.getMessage("delivery.boy.already.available", null));
				} else {
					throw new ValidationException(messageByLocaleService.getMessage("delivery.boy.already.unavailable", null));
				}
			}
			deliveryBoyCurrentStatus.setIsAvailable(isAvailable);
			deliveryBoyCurrentStatusRepository.save(deliveryBoyCurrentStatus);
			LOGGER.info("update is available for delivery boy :{} and isAvailable:{}", userLogin.getEntityId(), isAvailable);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
	}

	/**
	 * @param  userLogin
	 * @param  deliveryBoy
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
		 * if idle delivery boys is not available then go for a busy delivery boys who is going for delivery of orders(not for replacement or return) and at a
		 * time assigned order count is 1
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
	public synchronized void acceptOrder(final Long orderId) throws NotFoundException, ValidationException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (UserType.DELIVERY_BOY.name().equals(userLogin.getEntityType())) {
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
			DeliveryBoy deliveryBoy = getDeliveryBoyDetail(userLogin.getEntityId());
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
			taskDto.setDeliveryBoyId(deliveryBoy.getId());
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
		} else {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
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

	protected String getVendorAddress(final Vendor vendor) {
		Locale locale = LocaleContextHolder.getLocale();
		StringBuilder address = new StringBuilder();
		if (locale.getLanguage().equals("en")) {
			address.append(vendor.getBlockEnglish()).append(",").append(vendor.getBuildingEnglish()).append(",").append(vendor.getStreetEnglish()).append(",")
					.append(vendor.getAreaEnglish()).append(",").append(vendor.getCity().getNameEnglish()).append(",");
		} else {
			address.append(vendor.getBlockArabic()).append(",").append(vendor.getBuildingArabic()).append(",").append(vendor.getStreetArabic()).append(",")
					.append(vendor.getAreaArabic()).append(",").append(vendor.getCity().getNameArabic()).append(",");
		}
		address.append(vendor.getPincode().getCodeValue());
		return address.toString();
	}

	@Override
	public OrdersCountDTO getOrdersCount(final Long deliveryBoyId, final TaskFilterDTO taskFilterDTO) throws NotFoundException, ValidationException {
		OrdersCountDTO ordersCountDTO = new OrdersCountDTO();
		Map<String, Integer> assignedOrdersCountMap = new HashMap<>();
		/**
		 * regular orders
		 */

		taskFilterDTO.setDeliveryBoyId(deliveryBoyId);
		taskFilterDTO.setTaskType(TaskTypeEnum.DELIVERY.getTaskValue());
		Long regularOrders = taskService.getTaskCountBasedOnParams(taskFilterDTO);
		assignedOrdersCountMap.put("Regular Orders", regularOrders.intValue());
		ordersCountDTO.setDeliveryBoyId(deliveryBoyId);
		ordersCountDTO.setOrdersCountMap(assignedOrdersCountMap);
		/**
		 * set return/replace order count here
		 */
		assignedOrdersCountMap.put("Return Orders", regularOrders.intValue());
		assignedOrdersCountMap.put("Replace Orders", regularOrders.intValue());

		return ordersCountDTO;
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
			dashBoardDetailDTO.setOnGoingTaskId(task.getId());
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

	@Override
	public Long getDeliveryBoyCountBasedOnParams(final DeliveryBoyFilterDTO deliveryBoyFilterDTO) {
		return deliveryBoyRepository.getDeliveryBoyCountBasedOnParams(deliveryBoyFilterDTO);
	}

	@Override
	public List<DeliveryBoy> getDeliveryBoyListBasedOnParams(final Integer startIndex, final Integer pageSize, final DeliveryBoyFilterDTO deliveryBoyFilterDTO)
			throws ValidationException {
		sortByFieldAndDirection(deliveryBoyFilterDTO);
		return deliveryBoyRepository.getDeliveryBoyListBasedOnParams(startIndex, pageSize, deliveryBoyFilterDTO);
	}

	@Override
	public OrdersDetailDTOForDeliveryBoy getOrderDetailInDeliveryBoyAcceptNotification(final Long orderId) throws NotFoundException, ValidationException {
		return getOrderDetails(null, orderId);
	}

	@Override
	public OrdersDetailDTOForDeliveryBoy getOrderDetails(final Long taskId, final Long orderId) throws NotFoundException, ValidationException {
		if ((orderId == null && taskId == null) || (orderId != null && taskId != null)) {
			throw new ValidationException(messageByLocaleService.getMessage("order.id.task.id.not.null", null));
		}
		Locale locale = LocaleContextHolder.getLocale();
		Orders orders;
		OrdersDetailDTOForDeliveryBoy ordersDetailDTOForDeliveryBoy = new OrdersDetailDTOForDeliveryBoy();
		if (taskId != null) {
			Task task = taskService.getTaskDetail(taskId);
			orders = task.getOrder();
			ordersDetailDTOForDeliveryBoy.setTaskId(taskId);
			ordersDetailDTOForDeliveryBoy.setTaskStatus(task.getStatus());
			ordersDetailDTOForDeliveryBoy.setDeliveryDate(task.getDeliveredDate());
			if (locale.getLanguage().equals("en")) {
				ordersDetailDTOForDeliveryBoy.setStoreName(orders.getVendor().getStoreNameEnglish());
				ordersDetailDTOForDeliveryBoy
						.setDeliveryBoyName(task.getDeliveryBoy().getFirstNameEnglish() + " " + task.getDeliveryBoy().getLastNameEnglish());
			} else {
				ordersDetailDTOForDeliveryBoy.setStoreName(orders.getVendor().getStoreNameArabic());
				ordersDetailDTOForDeliveryBoy.setDeliveryBoyName(task.getDeliveryBoy().getFirstNameArabic() + " " + task.getDeliveryBoy().getLastNameArabic());
			}
			ordersDetailDTOForDeliveryBoy.setDeliveryBoyPhoneNumber(task.getDeliveryBoy().getPhoneNumber());
			ordersDetailDTOForDeliveryBoy.setDeliveryBoyEmail(task.getDeliveryBoy().getEmail());

		} else {
			orders = ordersService.getOrderById(orderId);
		}
		BeanUtils.copyProperties(orders, ordersDetailDTOForDeliveryBoy);
		ordersDetailDTOForDeliveryBoy.setCustomerEmail(orders.getCustomer().getEmail());
		ordersDetailDTOForDeliveryBoy.setCustomerName(orders.getFirstName() + " " + orders.getLastName());
		ordersDetailDTOForDeliveryBoy.setOrderRequest("New Order");
		ordersDetailDTOForDeliveryBoy.setDropLatitude(orders.getLatitude());
		ordersDetailDTOForDeliveryBoy.setDropLongitude(orders.getLongitude());
		ordersDetailDTOForDeliveryBoy.setDropContactNo(orders.getPhoneNumber());
		ordersDetailDTOForDeliveryBoy.setDropContactName(orders.getFirstName() + " " + orders.getLastName());
		ordersDetailDTOForDeliveryBoy.setPickupLatitude(orders.getVendor().getLatitude());
		ordersDetailDTOForDeliveryBoy.setPickupLongitude(orders.getVendor().getLongitude());
		ordersDetailDTOForDeliveryBoy.setPickupContactNo(orders.getVendor().getStorePhoneNumber());
		ordersDetailDTOForDeliveryBoy.setPickUpAddress(getVendorAddress(orders.getVendor()));
		if (locale.getLanguage().equals("en")) {
			ordersDetailDTOForDeliveryBoy.setDropAddress(orders.getAddressEnglish());
			ordersDetailDTOForDeliveryBoy.setPickupContactName(orders.getVendor().getStoreNameEnglish());
		} else {
			ordersDetailDTOForDeliveryBoy.setDropAddress(orders.getAddressArabic());
			ordersDetailDTOForDeliveryBoy.setPickupContactName(orders.getVendor().getStoreNameArabic());
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(orders.getVendor().getStoreImageName())) {
			ordersDetailDTOForDeliveryBoy.setStoreImageUrl(assetService.getGeneratedUrl(orders.getVendor().getStoreImageName(), AssetConstant.VENDOR));
		}

		List<OrderItemResponseDTO> orderItemResponseDTOList = orderItemService.getOrderItemResponseDTOForOrderId(orders.getId());
		ordersDetailDTOForDeliveryBoy.setOrderItemResponseDTOList(orderItemResponseDTOList);
		return ordersDetailDTOForDeliveryBoy;
	}

	@Override
	public List<OrdersListDTOForDeliveryBoy> getOrdersList(final Long deliveryBoyId, final Integer startIndex, final Integer pageSize,
			final TaskFilterDTO taskFilterDTO) throws NotFoundException {
		Locale locale = LocaleContextHolder.getLocale();
		getDeliveryBoyDetail(deliveryBoyId);
		List<Task> taskList = taskService.getTaskListBasedOnParams(taskFilterDTO, startIndex, pageSize);
		List<OrdersListDTOForDeliveryBoy> ordersListDTOsForDeliveryBoy = new ArrayList<>();
		for (Task task : taskList) {
			OrdersListDTOForDeliveryBoy ordersListDTOForDeliveryBoy = new OrdersListDTOForDeliveryBoy();
			ordersListDTOForDeliveryBoy.setId(task.getOrder().getId());
			ordersListDTOForDeliveryBoy.setTaskId(task.getId());
			ordersListDTOForDeliveryBoy.setTaskStatus(task.getStatus());
			ordersListDTOForDeliveryBoy.setOrderDate(task.getOrder().getCreatedAt());
			ordersListDTOForDeliveryBoy.setDeliveredDate(task.getDeliveredDate());
			ordersListDTOForDeliveryBoy.setOrderAmount(task.getOrder().getTotalOrderAmount());
			if (locale.getLanguage().equals("en")) {
				ordersListDTOForDeliveryBoy.setStoreName(task.getOrder().getVendor().getStoreNameEnglish());
			} else {
				ordersListDTOForDeliveryBoy.setStoreName(task.getOrder().getVendor().getStoreNameArabic());
			}
			if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(task.getOrder().getVendor().getStoreImageName())) {
				ordersListDTOForDeliveryBoy
						.setStoreImageUrl(assetService.getGeneratedUrl(task.getOrder().getVendor().getStoreImageName(), AssetConstant.VENDOR));
			}
			ordersListDTOsForDeliveryBoy.add(ordersListDTOForDeliveryBoy);
		}
		return ordersListDTOsForDeliveryBoy;
	}

}
