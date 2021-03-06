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
import java.util.stream.Collectors;

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
import com.nice.constant.PaymentMode;
import com.nice.constant.Role;
import com.nice.constant.SendingType;
import com.nice.constant.TaskStatusEnum;
import com.nice.constant.TaskTypeEnum;
import com.nice.constant.UserOtpTypeEnum;
import com.nice.constant.UserType;
import com.nice.dto.CompanyResponseDTO;
import com.nice.dto.DashBoardDetailDTO;
import com.nice.dto.DeliveryBoyAccountDetailsDTO;
import com.nice.dto.DeliveryBoyDTO;
import com.nice.dto.DeliveryBoyFilterDTO;
import com.nice.dto.DeliveryBoyLocationDTO;
import com.nice.dto.DeliveryBoyPersonalDetailsDTO;
import com.nice.dto.DeliveryBoyResponseDTO;
import com.nice.dto.Notification;
import com.nice.dto.OrderItemResponseDTO;
import com.nice.dto.OrdersCountDTO;
import com.nice.dto.OrdersDetailDTOForDeliveryBoy;
import com.nice.dto.OrdersListDTOForDeliveryBoy;
import com.nice.dto.PushNotificationDTO;
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
import com.nice.service.CompanyService;
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

	@Autowired
	private CompanyService companyService;

	@Override
	public DeliveryBoyResponseDTO addDeliveryBoy(final DeliveryBoyDTO deliveryBoyDTO, final MultipartFile profilePicture)
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
				/**
				 * re-send email only
				 */
				DeliveryBoyResponseDTO deliveryBoyResponseDTO = deliveryBoyMapper.toDto(deliveryBoy);
				deliveryBoyResponseDTO.setUserId(optUserLogin.get().getId());
				return deliveryBoyResponseDTO;
			}
		}
		/**
		 * Set delivery boy preferred language to default language when delivery boy registers.
		 */
		deliveryBoy.setPreferredLanguage(LocaleContextHolder.getLocale().getLanguage());

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

		CompanyResponseDTO company = companyService.getCompany(false);
		/**
		 * set default location details
		 */
		DeliveryBoyLocationDTO deliveryBoyLocationDTO = new DeliveryBoyLocationDTO();
		deliveryBoyLocationDTO.setDeliveryBoyId(deliveryBoy.getId());
		deliveryBoyLocationDTO.setLatitude(company.getLatitude());
		deliveryBoyLocationDTO.setLongitude(company.getLongitude());
		deliveryBoyLocationService.addUpdateDeliveryBoyLocation(deliveryBoyLocationDTO);

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
		DeliveryBoyResponseDTO deliveryBoyResponseDTO = deliveryBoyMapper.toDto(deliveryBoy);
		deliveryBoyResponseDTO.setUserId(userLogin.getId());
		return deliveryBoyResponseDTO;
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
				&& !Constant.SORT_DIRECTION_ASC.equals(deliveryBoyFilterDTO.getSortByDirection())
				&& !Constant.SORT_DIRECTION_DESC.equals(deliveryBoyFilterDTO.getSortByDirection())) {
			throw new ValidationException(messageByLocaleService.getMessage("sort.direction.invalid", null));
		}
	}

	@Override
	public DeliveryBoyCurrentStatus getDeliveryBoyCurrentStatusDetail(final DeliveryBoy deliveryBoy) throws NotFoundException {
		return deliveryBoyCurrentStatusRepository.findByDeliveryBoy(deliveryBoy).orElseThrow(
				() -> new NotFoundException(messageByLocaleService.getMessage("deliveryboy.current.status.not.found", new Object[] { deliveryBoy.getId() })));
	}

	@Override
	public void exportList(final DeliveryBoyFilterDTO deliveryBoyFilterDTO, final HttpServletResponse httpServletResponse)
			throws FileNotFoundException, ValidationException {
		sortByFieldAndDirection(deliveryBoyFilterDTO);
		List<DeliveryBoy> deliveryBoyList = deliveryBoyRepository.getDeliveryBoyListBasedOnParams(null, null, deliveryBoyFilterDTO);
		List<DeliveryBoyResponseDTO> deliveryBoyDtoList = new ArrayList<>();
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
				userName = userLogin.get().getEmail();
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
		} else {
			throw new NotFoundException(messageByLocaleService.getMessage("user.not.exists.email", new Object[] { deliveryBoy.getEmail() }));
		}
		return userName;
	}

	@Override
	public void updateProfilePicture(final MultipartFile profilePicture) throws NotFoundException, ValidationException, FileOperationException {
		Long deliveryBoyId = getDeliveryBoyIdFromToken();
		LOGGER.info("Inside update profile picture of delivery boy id:{}", deliveryBoyId);
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
		Long deliveryBoyId = getDeliveryBoyIdFromToken();
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		DeliveryBoyCurrentStatus deliveryBoyCurrentStatus = getDeliveryBoyCurrentStatusDetail(getDeliveryBoyDetail(deliveryBoyId));
		if (deliveryBoyCurrentStatus.getIsAvailable().equals(isAvailable)) {
			if (isAvailable.booleanValue()) {
				throw new ValidationException(messageByLocaleService.getMessage("delivery.boy.already.available", null));
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("delivery.boy.already.unavailable", null));
			}
		}
		if (Boolean.TRUE.equals(isAvailable)) {
			/**
			 * if delivery boy's device detail is not present then can not be available for accept order
			 */
			Optional<List<DeviceDetail>> deviceDetailList = deviceDetailService.getDeviceDetailListByUserId(userLogin.getId());
			if (!deviceDetailList.isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("deliveryboy.device.detail.required.active", null));
			}
		} else {
			/**
			 * if delivery boy has on going order which is not delivered yet then can not set is available to false
			 */
			TaskFilterDTO taskFilterDTO = new TaskFilterDTO();
			taskFilterDTO.setDeliveryBoyId(deliveryBoyId);
			taskFilterDTO.setStatusListNotIn(Arrays.asList(TaskStatusEnum.ORDER_ACCEPTED.getStatusValue(), TaskStatusEnum.DELIVERED.getStatusValue(),
					TaskStatusEnum.CANCELLED.getStatusValue()));
			Long count = taskService.getTaskCountBasedOnParams(taskFilterDTO, false);
			if (count > 0) {
				throw new ValidationException(messageByLocaleService.getMessage("deliver.order.first", null));
			}
		}

		deliveryBoyCurrentStatus.setIsAvailable(isAvailable);
		deliveryBoyCurrentStatusRepository.save(deliveryBoyCurrentStatus);
		LOGGER.info("update is available for delivery boy :{} and isAvailable:{}", deliveryBoyId, isAvailable);
	}

	@Override
	public Long getDeliveryBoyIdFromToken() throws ValidationException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (!UserType.DELIVERY_BOY.name().equals(userLogin.getEntityType())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		} else {
			return userLogin.getEntityId();
		}
	}

	/**
	 * @param  userLogin
	 * @param  deliveryBoy
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	@Override
	public void sendOtpForEmailVerification(final DeliveryBoyResponseDTO deliveryBoyResponseDTO) throws NotFoundException, ValidationException {
		UserOtpDto userOtpDto = new UserOtpDto();
		userOtpDto.setEmail(deliveryBoyResponseDTO.getEmail());
		userOtpDto.setType(UserOtpTypeEnum.EMAIL.name());
		userOtpDto.setUserId(deliveryBoyResponseDTO.getUserId());
		UserOtp otp = otpService.generateOtp(userOtpDto);
		sendEmail(otp.getOtp(), deliveryBoyResponseDTO);
	}

	private void sendEmail(final String otp, final DeliveryBoyResponseDTO deliveryBoyResponseDTO) {
		LOGGER.info("Inside sendEmail "+ deliveryBoyResponseDTO.getEmail());
		Notification notification = new Notification();
		notification.setOtp(otp);
		notification.setUserId(deliveryBoyResponseDTO.getUserId());
		notification.setEmail(deliveryBoyResponseDTO.getEmail());
		notification.setDeliveryBoyId(deliveryBoyResponseDTO.getId());
		notification.setUserType(UserType.DELIVERY_BOY.name());
		notification.setSendingType(SendingType.OTP.name());
		notification.setType(NotificationQueueConstants.EMAIL_VERIFICATION);
		notification.setLanguage(deliveryBoyResponseDTO.getPreferredLanguage());
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
		 * if idle delivery boys is not available then go for a busy delivery boys who is going for delivery of orders and at a
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
			taskFilterDTO.setStatusListNotIn(Arrays.asList(TaskStatusEnum.DELIVERED.getStatusValue(), TaskStatusEnum.CANCELLED.getStatusValue()));
			Long count = taskService.getTaskCountBasedOnParams(taskFilterDTO, true);
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
			DeliveryBoyLocation deliveryBoyLocation = deliveryBoyLocationService.getDeliveryBoyLocationByDeliveryBoyId(deliveryBoy.getId());
			Double distance = CommonUtility.distance(vendor.getLatitude().doubleValue(), vendor.getLongitude().doubleValue(),
					deliveryBoyLocation.getLatitude().doubleValue(), deliveryBoyLocation.getLongitude().doubleValue());
			/**
			 * if delivery boy's distance is less then max distance from vendor then only consider him
			 */
			if (distance <= Constant.MAX_DISTANCE_FROM_VENDOR) {
				deliveryBoyWithDistanceMap.put(deliveryBoy.getId(), distance);
			}
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
	public synchronized void acceptOrder(final Long orderId, final String taskType) throws NotFoundException, ValidationException {
		Long deliveryBoyId = getDeliveryBoyIdFromToken();
		/**
		 * check is order already accepted then throw exception else set delivery boy in order
		 */
		Orders orders = ordersService.getOrderById(orderId);
		if (!OrderStatusEnum.CONFIRMED.getStatusValue().equals(orders.getOrderStatus())
				&& !OrderStatusEnum.RETURN_CONFIRMED.getStatusValue().equals(orders.getOrderStatus())
				&& !OrderStatusEnum.REPLACE_CONFIRMED.getStatusValue().equals(orders.getOrderStatus())) {
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
		String nextOrderStatus;
		if (TaskTypeEnum.DELIVERY.getTaskValue().equals(taskType)) {
			orders.setDeliveryBoy(deliveryBoy);
			nextOrderStatus = Constant.IN_PROCESS;
		} else if (TaskTypeEnum.RETURN.getTaskValue().equals(taskType)) {
			orders.setReplacementDeliveryBoy(deliveryBoy);
			nextOrderStatus = Constant.RETURN_PROCESSED;
		} else if (TaskTypeEnum.REPLACEMENT.getTaskValue().equals(taskType)) {
			orders.setReplacementDeliveryBoy(deliveryBoy);
			nextOrderStatus = Constant.REPLACE_PROCESSED;
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.task.status", null));
		}

		ordersService.changeStatus(nextOrderStatus, orders);

		/**
		 * create a delivery task for delivery boy
		 */
		TaskDto taskDto = new TaskDto();
		taskDto.setDeliveryBoyId(deliveryBoy.getId());
		taskDto.setOrderId(orders.getId());
		taskDto.setTaskType(taskType);
		taskDto.setStatus(TaskStatusEnum.ORDER_ACCEPTED.getStatusValue());
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
		Long deliveryBoyId = getDeliveryBoyIdFromToken();
		DeliveryBoyCurrentStatus deliveryBoyCurrentStatus = getDeliveryBoyCurrentStatusDetail(getDeliveryBoyDetail(deliveryBoyId));
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

	@Override
	public synchronized void updateDeliveryBoyRating(final Long deliveryBoyId, final Double ratingByClient) throws NotFoundException {
		DeliveryBoy deliveryBoy = getDeliveryBoyDetail(deliveryBoyId);
		Double updatedRating = (deliveryBoy.getRating() * deliveryBoy.getNoOfRating() + ratingByClient) / (deliveryBoy.getNoOfRating() + 1);
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
			address.append(vendor.getBuildingEnglish()).append(",").append(vendor.getBlockEnglish()).append(",").append(vendor.getStreetEnglish()).append(",")
					.append(vendor.getArea().getNameEnglish()).append(",").append(vendor.getCountry().getNameEnglish());
		} else {
			address.append(vendor.getBuildingArabic()).append(",").append(vendor.getBlockArabic()).append(",").append(vendor.getStreetArabic()).append(",")
					.append(vendor.getArea().getNameArabic()).append(",").append(vendor.getCountry().getNameArabic());
		}
		return address.toString();
	}

	@Override
	public OrdersCountDTO getOrdersCount(final TaskFilterDTO taskFilterDTO) throws NotFoundException, ValidationException {
		OrdersCountDTO ordersCountDTO = new OrdersCountDTO();
		Long deliveryBoyId = getDeliveryBoyIdFromToken();
		LOGGER.info("Inside get orders count for id:{}", deliveryBoyId);
		Map<String, Integer> assignedOrdersCountMap = new HashMap<>();
		/**
		 * regular orders
		 */

		taskFilterDTO.setDeliveryBoyId(deliveryBoyId);
		taskFilterDTO.setTaskType(TaskTypeEnum.DELIVERY.getTaskValue());
		Long regularOrders = taskService.getTaskCountBasedOnParams(taskFilterDTO, false);
		assignedOrdersCountMap.put("Regular Orders", regularOrders.intValue());
		/**
		 * set return order count
		 */
		taskFilterDTO.setTaskType(TaskTypeEnum.RETURN.getTaskValue());
		Long returnOrders = taskService.getTaskCountBasedOnParams(taskFilterDTO, false);
		assignedOrdersCountMap.put("Return Orders", returnOrders.intValue());
		/**
		 * set replace order count
		 */
		taskFilterDTO.setTaskType(TaskTypeEnum.REPLACEMENT.getTaskValue());
		Long replaceOrders = taskService.getTaskCountBasedOnParams(taskFilterDTO, false);
		assignedOrdersCountMap.put("Replace Orders", replaceOrders.intValue());

		ordersCountDTO.setDeliveryBoyId(deliveryBoyId);
		ordersCountDTO.setOrdersCountMap(assignedOrdersCountMap);
		return ordersCountDTO;
	}

	@Override
	public DashBoardDetailDTO getDashBoard() throws NotFoundException, ValidationException {
		Long deliveryBoyId = getDeliveryBoyIdFromToken();
		LOGGER.info("Inside get dash board for id:{}", deliveryBoyId);
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
		taskFilterDTO.setStatusListNotIn(Arrays.asList(TaskStatusEnum.DELIVERED.getStatusValue(), TaskStatusEnum.CANCELLED.getStatusValue()));
		Long count = taskService.getTaskCountBasedOnParams(taskFilterDTO, false);
		dashBoardDetailDTO.setAssignedOrdersCount(count.intValue());
		/**
		 * today's delivered orders count
		 */
		taskFilterDTO.setStatusListNotIn(null);
		taskFilterDTO.setTaskType(null);
		taskFilterDTO.setStatusList(Arrays.asList(TaskStatusEnum.DELIVERED.getStatusValue(), TaskStatusEnum.CANCELLED.getStatusValue()));
		taskFilterDTO.setDeliveredDate(new Date(System.currentTimeMillis()));
		count = taskService.getTaskCountBasedOnParams(taskFilterDTO, false);
		dashBoardDetailDTO.setDeliveredOrdersCount(count.intValue());
		/**
		 * for on going order
		 */
		taskFilterDTO.setStatusList(null);
		taskFilterDTO.setStatusListNotIn(Arrays.asList(TaskStatusEnum.ORDER_ACCEPTED.getStatusValue(), TaskStatusEnum.DELIVERED.getStatusValue(),
				TaskStatusEnum.CANCELLED.getStatusValue()));
		taskFilterDTO.setDeliveredDate(null);
		List<Task> taskList = taskService.getTaskListBasedOnParams(taskFilterDTO, null, null);
		if (taskList.size() > 1) {
			throw new ValidationException("morethen.one.ongoing.order", null);
		}
		for (Task task : taskList) {
			dashBoardDetailDTO.setOnGoingOrderId(task.getOrder().getId());
			dashBoardDetailDTO.setOnGoingTaskId(task.getId());
			dashBoardDetailDTO.setOrderType(task.getTaskType());
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
		if (orderId == null && taskId == null || orderId != null && taskId != null) {
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
			ordersDetailDTOForDeliveryBoy.setTaskType(task.getTaskType());
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
			/**
			 * Cash collected or not
			 */
			if (PaymentMode.COD.name().equals(orders.getPaymentMode())) {
				ordersDetailDTOForDeliveryBoy.setCashCollected(cashCollectionService.getCashCollectionDetailForTask(taskId).isPresent());
			}
			/**
			 * Next status
			 */
			final TaskStatusEnum taskOldStatus = TaskStatusEnum.valueOf(TaskStatusEnum.getByValue(task.getStatus()).name());
			if (TaskTypeEnum.DELIVERY.getTaskValue().equals(task.getTaskType())) {
				if (taskOldStatus.nextStatusForMobile() != null && taskOldStatus.nextStatusForMobile().length > 0) {
					final List<TaskStatusEnum> taskStatusList = Arrays.asList(taskOldStatus.nextStatusForMobile());
					ordersDetailDTOForDeliveryBoy
							.setNextStatus(taskStatusList.stream().map(TaskStatusEnum::getStatusValue).collect(Collectors.toList()).get(0));
				}
			} else if (TaskTypeEnum.RETURN.getTaskValue().equals(task.getTaskType())) {
				if (taskOldStatus.nextReturnOrderTaskStatusForMobile() != null && taskOldStatus.nextReturnOrderTaskStatusForMobile().length > 0) {
					final List<TaskStatusEnum> taskStatusList = Arrays.asList(taskOldStatus.nextReturnOrderTaskStatusForMobile());
					ordersDetailDTOForDeliveryBoy
							.setNextStatus(taskStatusList.stream().map(TaskStatusEnum::getStatusValue).collect(Collectors.toList()).get(0));
				}
			} else {
				if (taskOldStatus.nextReplaceOrderTaskStatusForMobile() != null && taskOldStatus.nextReplaceOrderTaskStatusForMobile().length > 0) {
					final List<TaskStatusEnum> taskStatusList = Arrays.asList(taskOldStatus.nextReplaceOrderTaskStatusForMobile());
					ordersDetailDTOForDeliveryBoy
							.setNextStatus(taskStatusList.stream().map(TaskStatusEnum::getStatusValue).collect(Collectors.toList()).get(0));
				}
			}
		} else {
			orders = ordersService.getOrderById(orderId);
		}
		BeanUtils.copyProperties(orders, ordersDetailDTOForDeliveryBoy);
		ordersDetailDTOForDeliveryBoy.setCustomerEmail(orders.getCustomer().getEmail());
		ordersDetailDTOForDeliveryBoy.setCustomerName(orders.getFirstName() + " " + orders.getLastName());
		if (Constant.normalOrderStatusList().contains(orders.getOrderStatus())) {
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
		} else {
			if (Constant.returnOrderStatusList().contains(orders.getOrderStatus())) {
				ordersDetailDTOForDeliveryBoy.setOrderRequest("Return Order");
			} else {
				ordersDetailDTOForDeliveryBoy.setOrderRequest("Replacement");
			}
			ordersDetailDTOForDeliveryBoy.setDropLatitude(orders.getVendor().getLatitude());
			ordersDetailDTOForDeliveryBoy.setDropLongitude(orders.getVendor().getLongitude());
			ordersDetailDTOForDeliveryBoy.setDropContactNo(orders.getVendor().getStorePhoneNumber());
			ordersDetailDTOForDeliveryBoy.setPickupLatitude(orders.getLatitude());
			ordersDetailDTOForDeliveryBoy.setPickupLongitude(orders.getLongitude());
			ordersDetailDTOForDeliveryBoy.setPickupContactNo(orders.getPhoneNumber());
			ordersDetailDTOForDeliveryBoy.setDropAddress(getVendorAddress(orders.getVendor()));
			ordersDetailDTOForDeliveryBoy.setPickupContactName(orders.getFirstName() + " " + orders.getLastName());
			if (locale.getLanguage().equals("en")) {
				ordersDetailDTOForDeliveryBoy.setDropContactName(orders.getVendor().getStoreNameEnglish());
				ordersDetailDTOForDeliveryBoy.setPickUpAddress(orders.getAddressEnglish());
			} else {
				ordersDetailDTOForDeliveryBoy.setDropContactName(orders.getVendor().getStoreNameArabic());
				ordersDetailDTOForDeliveryBoy.setPickUpAddress(orders.getAddressEnglish());

			}
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

	@Override
	public Long getCountOfOnFieldDeliveryBoy() {
		return deliveryBoyRepository.countByIsAvailableAndActive(true, true);
	}

	@Override
	public Long getCountOfNewDeliveryBoys() {
		return deliveryBoyRepository.getCountOfNewDeliveryBoys(DeliveryBoyStatus.PENDING.getStatusValue(), DeliveryBoyStatus.VERIFIED.getStatusValue());
	}

	@Override
	public void sendPushNotification(final String acceptOrderPushNotificationCustomer, final Long orderId) throws NotFoundException {
		Orders orders = ordersService.getOrderById(orderId);
		PushNotificationDTO pushNotificationDTO = new PushNotificationDTO();
		pushNotificationDTO.setModule(Constant.ORDER_MODULE);
		pushNotificationDTO.setOrderId(orderId);
		pushNotificationDTO.setCustomerId(orders.getCustomer().getId());
		pushNotificationDTO.setType(acceptOrderPushNotificationCustomer);
		pushNotificationDTO
				.setDeliveryBoyId(orders.getReplacementDeliveryBoy() != null ? orders.getReplacementDeliveryBoy().getId() : orders.getDeliveryBoy().getId());
		jmsQueuerService.sendPushNotification(NotificationQueueConstants.GENERAL_PUSH_NOTIFICATION_QUEUE, pushNotificationDTO);
	}

	@Override
	public void sendEmailAfterAccountActivation(final Long deliveryBoyId) {
		Notification notification = new Notification();
		notification.setDeliveryBoyId(deliveryBoyId);
		notification.setType(NotificationQueueConstants.DELIVERY_BOY_ACCOUNT_ACTIVATION);
		jmsQueuerService.sendEmail(NotificationQueueConstants.NON_NOTIFICATION_QUEUE, notification);
	}

	@Override
	public Long getAllDeliveryBoyCount() {
		return deliveryBoyRepository.count();
	}
}
