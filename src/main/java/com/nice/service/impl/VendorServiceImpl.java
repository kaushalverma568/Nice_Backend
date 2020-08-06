package com.nice.service.impl;

import java.io.IOException;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.mail.MessagingException;
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
import com.nice.constant.DeliveryType;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.PaymentMethod;
import com.nice.constant.Role;
import com.nice.constant.SendingType;
import com.nice.constant.UserOtpTypeEnum;
import com.nice.constant.UserType;
import com.nice.constant.VendorAccepts;
import com.nice.constant.VendorStatus;
import com.nice.dto.Notification;
import com.nice.dto.UserOtpDto;
import com.nice.dto.VendorBankDetailsDTO;
import com.nice.dto.VendorBasicDetailDTO;
import com.nice.dto.VendorCuisineDTO;
import com.nice.dto.VendorDTO;
import com.nice.dto.VendorExport;
import com.nice.dto.VendorFilterDTO;
import com.nice.dto.VendorListFilterDTO;
import com.nice.dto.VendorResponseDTO;
import com.nice.dto.VendorRestaurantDetailsDTO;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.jms.queue.JMSQueuerService;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.VendorMapper;
import com.nice.model.BusinessCategory;
import com.nice.model.City;
import com.nice.model.Country;
import com.nice.model.CustomerAddress;
import com.nice.model.Pincode;
import com.nice.model.SubscriptionPlan;
import com.nice.model.UserLogin;
import com.nice.model.UserOtp;
import com.nice.model.Vendor;
import com.nice.model.VendorBankDetails;
import com.nice.model.VendorCuisine;
import com.nice.repository.VendorBankDetailsRepository;
import com.nice.repository.VendorRepository;
import com.nice.service.AssetService;
import com.nice.service.BusinessCategoryService;
import com.nice.service.CityService;
import com.nice.service.CountryService;
import com.nice.service.CustomerAddressService;
import com.nice.service.OtpService;
import com.nice.service.PincodeService;
import com.nice.service.SubscriptionPlanService;
import com.nice.service.UserLoginService;
import com.nice.service.VendorCuisineService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;
import com.nice.util.ExportCSV;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Mar-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("vendorService")
public class VendorServiceImpl implements VendorService {

	/**
	 *
	 */
	private static final String VENDOR_ACTIVE_FIRST = "vendor.active.first";

	private static final Logger LOGGER = LoggerFactory.getLogger(VendorServiceImpl.class);

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private UserLoginService userLoginService;

	@Autowired
	private CountryService countryService;

	@Autowired
	private CityService cityService;

	@Autowired
	private PincodeService pincodeService;

	@Autowired
	private VendorRepository vendorRepository;

	@Autowired
	private VendorBankDetailsRepository vendorBankDetailsRepository;

	@Autowired
	private VendorMapper vendorMapper;

	@Autowired
	private BusinessCategoryService businessCategoryService;

	@Autowired
	private SubscriptionPlanService subscriptionPlanService;

	@Autowired
	private OtpService otpService;

	@Autowired
	private JMSQueuerService jmsQueuerService;

	@Autowired
	private VendorCuisineService vendorCuisineService;

	@Autowired
	private CustomerAddressService customerAddressService;

	@Autowired
	private ExportCSV exportCSV;

	@Autowired
	private AssetService assetService;

	@Override
	public void addVendor(final VendorDTO vendorDTO) throws ValidationException, NotFoundException {
		if (!CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendorDTO.getPassword())) {
			throw new ValidationException(messageByLocaleService.getMessage("password.required", null));
		}

		/**
		 * Check if vendor already exists, if so then lets only send him email again.
		 */
		Optional<Vendor> optVendor = vendorRepository.findByEmail(vendorDTO.getEmail().toLowerCase());
		if (optVendor.isPresent() && !optVendor.get().getEmailVerified().booleanValue()) {
			Optional<UserLogin> optUserLogin = userLoginService.getUserLoginBasedOnEmailAndEntityType(optVendor.get().getEmail(), UserType.VENDOR.name());
			if (optUserLogin.isPresent()) {
				sendOtpForEmailVerification(optUserLogin.get(), optVendor.get());
				return;
			}
		}

		Vendor vendor = vendorMapper.toEntity(vendorDTO);
		BusinessCategory businessCategory = businessCategoryService.getBusinessCategoryDetail(vendorDTO.getBusinessCategoryId());
		Country country = countryService.getCountryDetails(vendorDTO.getCountryId());
		City city = cityService.getCityDetails(vendorDTO.getCityId());
		Pincode pincode = pincodeService.getPincodeDetails(vendorDTO.getPincodeId());
		/**
		 * at the time of creation status will be verification pending
		 */
		vendor.setStatus(VendorStatus.VERIFICATION_PENDING.getStatusValue());
		vendor.setEmailVerified(false);
		vendor.setPhoneVerified(false);
		vendor.setActive(false);
		vendor.setIsOrderServiceEnable(false);
		vendor.setBusinessCategory(businessCategory);
		vendor.setCity(city);
		vendor.setCountry(country);
		vendor.setPincode(pincode);
		vendor.setIsFeatured(false);
		vendor.setProfileCompleted(false);
		vendor = vendorRepository.save(vendor);
		/**
		 * set login details of vendor
		 */
		final UserLogin userLogin = new UserLogin();
		userLogin.setEntityId(vendor.getId());
		userLogin.setEntityType(UserType.VENDOR.name());
		userLogin.setEmail(vendor.getEmail());
		userLogin.setRole(Role.VENDOR.name());
		userLogin.setPassword(vendorDTO.getPassword());
		userLogin.setActive(false);
		userLoginService.addUserLogin(userLogin);
		LOGGER.info("Inside add Vendor service vendor:{}", vendor);

		/**
		 * Code to generate OTP and send that in email.
		 */
		sendOtpForEmailVerification(userLogin, vendor);
	}

	@Override
	public void updateBankDetails(final VendorBankDetailsDTO vendorBankDetailsDTO) throws NotFoundException, ValidationException {
		Vendor vendor = getVendorDetail(vendorBankDetailsDTO.getVendorId());
		if (vendor.getAccepts() != null && vendor.getMinimumOrderAmt() != null) {
			if (VendorStatus.ACTIVE.getStatusValue().equals(vendor.getStatus()) && vendor.getActive().booleanValue()) {
				Optional<VendorBankDetails> existingVendorBankDetails = vendorBankDetailsRepository.findByVendor(vendor);
				VendorBankDetails vendorBankDetails = new VendorBankDetails();
				BeanUtils.copyProperties(vendorBankDetailsDTO, vendorBankDetails);
				if (existingVendorBankDetails.isPresent()) {
					vendorBankDetails.setId(existingVendorBankDetails.get().getId());
				}
				vendorBankDetails.setVendor(vendor);
				vendorBankDetails.setActive(true);
				vendorBankDetailsRepository.save(vendorBankDetails);
				vendor.setProfileCompleted(true);
				vendorRepository.save(vendor);
			} else {
				throw new ValidationException(messageByLocaleService.getMessage(VENDOR_ACTIVE_FIRST, null));
			}
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("vendor.update.restaurant.detail.first", null));
		}
	}

	@Override
	public VendorResponseDTO getVendor(final Long vendorId) throws NotFoundException {
		return vendorMapper.toDto(getVendorDetail(vendorId));
	}

	@Override
	public Vendor getVendorDetail(final Long vendorId) throws NotFoundException {
		return vendorRepository.findById(vendorId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("vendor.not.found", new Object[] { vendorId })));
	}

	@Override
	public VendorBankDetails getVendorBankDetails(final Long vendorId) throws NotFoundException {
		return vendorBankDetailsRepository.findByVendor(getVendorDetail(vendorId))
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("vendor.bank.details.found", new Object[] { vendorId })));
	}

	@Override
	public Page<Vendor> getVendorList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final Boolean isEmailVerified)
			throws NotFoundException {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("id"));
		if (isEmailVerified != null) {
			if (activeRecords != null) {
				return vendorRepository.findAllByActiveAndEmailVerified(activeRecords, isEmailVerified, pageable);
			} else {
				return vendorRepository.findAllByEmailVerified(isEmailVerified, pageable);
			}
		} else {
			if (activeRecords != null) {
				return vendorRepository.findAllByActive(activeRecords, pageable);
			} else {
				return vendorRepository.findAll(pageable);
			}
		}
	}

	@Override
	public String changeStatus(final Long vendorId, final Boolean active) throws NotFoundException, ValidationException {
		final Vendor vendor = getVendorDetail(vendorId);
		LOGGER.info("Existing Vendor details {} ", vendor);
		String userName = null;
		final Optional<UserLogin> userLogin = userLoginService.getUserLoginBasedOnEmailAndRole(vendor.getEmail(), Role.VENDOR.name());
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (vendor.getActive().equals(active)) {
			throw new ValidationException(
					messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "vendor.already.active" : "vendor.already.deactive", null));
		} else if (userLogin.isPresent()) {
			if (Boolean.FALSE.equals(active)) {
				/**
				 * if vendor's subscription is active then he can't be activated
				 */
				if (VendorStatus.ACTIVE.getStatusValue().equals(vendor.getStatus())) {
					throw new ValidationException(messageByLocaleService.getMessage("vendor.subscription.active", null));
				}
				/**
				 * deActive all vendor cuisines of this vendor
				 */
				List<VendorCuisine> vendorCuisineList = vendorCuisineService.getVendorCuisineListByVendor(vendorId, true);
				for (VendorCuisine vendorCuisine : vendorCuisineList) {
					vendorCuisineService.changeStatus(vendorCuisine.getId(), false);
				}
			} else {
				if (!vendor.getEmailVerified().booleanValue()) {
					throw new ValidationException(messageByLocaleService.getMessage("email.not.verified", null));
				}
				/**
				 * at time of active vendor value check business category is active or not
				 */
				if (Boolean.FALSE.equals(vendor.getBusinessCategory().getActive())) {
					throw new ValidationException(messageByLocaleService.getMessage("business.category.activate.first", null));
				}
			}
			userLogin.get().setActive(active);
			userLoginService.updateUserLogin(userLogin.get());
			vendor.setActive(active);
			vendorRepository.save(vendor);
			userName = userLogin.get().getEmail();
		} else {
			throw new NotFoundException(messageByLocaleService.getMessage("user.not.exists.email", new Object[] { vendor.getEmail() }));
		}
		return userName;
	}

	@Override
	public Boolean isVendorExists(final VendorDTO vendorDTO) {
		if (vendorDTO.getId() != null) {
			/**
			 * At the time of update is vendor with same email exist or not except it's own id
			 */
			return vendorRepository.findByEmailAndIdNot(vendorDTO.getEmail().toLowerCase(), vendorDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is vendor with same email exist or not
			 */
			Optional<Vendor> vendor = vendorRepository.findByEmail(vendorDTO.getEmail().toLowerCase());
			if (vendor.isPresent()) {
				/**
				 * If the vendor is present and his email not verified, then we will be sending the verification link for him again, if the email is verified
				 * then we will be returning true.
				 */
				return vendor.get().getEmailVerified();
			} else {
				return false;
			}
		}
	}

	@Override
	public Boolean isUserLoginExists(final VendorDTO vendorDTO) {
		Optional<Vendor> vendor = vendorRepository.findByEmail(vendorDTO.getEmail().toLowerCase());
		if (vendorDTO.getId() == null && vendor.isPresent()) {
			/**
			 * If the vendor is present and his email not verified, then we will be sending the verification link for him again, if the email is verified then
			 * we will be returning true.
			 */
			return vendor.get().getEmailVerified();
		}
		Optional<UserLogin> optUserLogin;
		try {
			optUserLogin = userLoginService.getUserLoginBasedOnUserNameAndUserType(vendorDTO.getEmail().toLowerCase(), UserType.USER.name());
		} catch (ValidationException e) {
			return true;
		}
		if (optUserLogin.isPresent()) {
			return !(vendorDTO.getId() != null && UserType.VENDOR.name().equals(optUserLogin.get().getEntityType())
					&& vendorDTO.getId().equals(optUserLogin.get().getEntityId()));
		} else {
			return false;
		}
	}

	@Override
	public void verifyEmail(final Long vendorId) throws NotFoundException {
		Vendor vendor = getVendorDetail(vendorId);
		/**
		 * if vendor is verifying his email for first time then his old status will verification pending
		 */
		if (VendorStatus.VERIFICATION_PENDING.getStatusValue().equals(vendor.getStatus())) {
			vendor.setStatus(VendorStatus.NEW.getStatusValue());
		}
		vendor.setEmailVerified(true);
		vendor.setActive(true);
		vendorRepository.save(vendor);
	}

	/**
	 * @param  userLogin
	 * @param  vendor
	 * @throws NotFoundException
	 * @throws ValidationException
	 * @throws MessagingException
	 */
	private void sendOtpForEmailVerification(final UserLogin userLogin, final Vendor vendor) throws NotFoundException, ValidationException {
		UserOtpDto userOtpDto = new UserOtpDto();
		userOtpDto.setEmail(vendor.getEmail());
		userOtpDto.setType(UserOtpTypeEnum.EMAIL.name());
		userOtpDto.setUserLoginId(userLogin.getId());
		UserOtp otp = otpService.generateOtp(userOtpDto);
		sendEmail(otp.getOtp(), userLogin.getId(), vendor.getEmail());
	}

	private void sendEmail(final String otp, final Long userId, final String email) {
		Notification notification = new Notification();
		notification.setOtp(otp);
		notification.setUserId(userId);
		notification.setEmail(email);
		/**
		 * we will send verification link only
		 */
		notification.setSendingType(SendingType.LINK.name());
		notification.setType(NotificationQueueConstants.EMAIL_VERIFICATION);
		jmsQueuerService.sendEmail(NotificationQueueConstants.NON_NOTIFICATION_QUEUE, notification);
	}

	@Override
	public void updatePersonalDetails(final VendorDTO vendorDTO) throws NotFoundException, ValidationException {
		if (vendorDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("vendor.id.not.null", null));
		}
		Vendor existingVendor = getVendorDetail(vendorDTO.getId());
		if (existingVendor.getActive().booleanValue() && VendorStatus.ACTIVE.getStatusValue().equals(existingVendor.getStatus())) {
			Vendor vendor = vendorMapper.toEntity(vendorDTO);
			if (!existingVendor.getBusinessCategory().getId().equals(vendorDTO.getBusinessCategoryId())) {
				throw new ValidationException(messageByLocaleService.getMessage("vendor.business.category.mismatch", null));
			}
			if (!existingVendor.getEmail().equals(vendorDTO.getEmail())) {
				throw new ValidationException(messageByLocaleService.getMessage("vendor.email.mismatch", null));
			} else {
				vendor.setEmailVerified(existingVendor.getEmailVerified());
				vendor.setIsOrderServiceEnable(existingVendor.getIsOrderServiceEnable());
				vendor.setActive(existingVendor.getActive());
			}
			if (!existingVendor.getPhoneNumber().equals(vendorDTO.getPhoneNumber())) {
				throw new ValidationException(messageByLocaleService.getMessage("vendor.contact.mismatch", null));
			} else {
				vendor.setPhoneVerified(existingVendor.getPhoneVerified());
			}
			if (!existingVendor.getPhoneNumber().equals(vendorDTO.getPhoneNumber())) {
				/**
				 * send otp for contact verification
				 */
				vendor.setPhoneVerified(false);
			} else {
				vendor.setPhoneVerified(existingVendor.getPhoneVerified());
			}
			BusinessCategory businessCategory = businessCategoryService.getBusinessCategoryDetail(vendorDTO.getBusinessCategoryId());
			Country country = countryService.getCountryDetails(vendorDTO.getCountryId());
			City city = cityService.getCityDetails(vendorDTO.getCityId());
			Pincode pincode = pincodeService.getPincodeDetails(vendorDTO.getPincodeId());
			vendor.setBusinessCategory(businessCategory);
			vendor.setCity(city);
			vendor.setCountry(country);
			vendor.setPincode(pincode);
			vendor.setRating(existingVendor.getRating());
			vendor.setNoOfRating(existingVendor.getNoOfRating());
			vendor.setSubscriptionPlan(existingVendor.getSubscriptionPlan());
			vendor.setSubscriptionPlanStartDate(existingVendor.getSubscriptionPlanStartDate());
			vendor.setSubscriptionPlanEndDate(existingVendor.getSubscriptionPlanEndDate());
			vendor.setStatus(existingVendor.getStatus());
			vendorRepository.save(vendor);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage(VENDOR_ACTIVE_FIRST, null));
		}

	}

	@Override
	public void addUpdateSubscriptionPlan(final Long vendorId, final Long subscriptionPlanId) throws NotFoundException, ValidationException {
		Vendor vendor = getVendorDetail(vendorId);
		if (VendorStatus.APPROVED.getStatusValue().equals(vendor.getStatus()) || VendorStatus.EXPIRED.getStatusValue().equals(vendor.getStatus())) {
			SubscriptionPlan subscriptionPlan = subscriptionPlanService.getSubscriptionPlanDetail(subscriptionPlanId);
			vendor.setSubscriptionPlanStartDate(new Date(System.currentTimeMillis()));
			vendor.setSubscriptionPlan(subscriptionPlan);
			/**
			 * add subscription duration days from subscription start date
			 */
			vendor.setSubscriptionPlanEndDate(Date.from(CommonUtility.convetUtilDatetoLocalDate(vendor.getSubscriptionPlanStartDate())
					.plusDays(subscriptionPlan.getDays()).atStartOfDay().atZone(ZoneId.systemDefault()).toInstant()));
			vendor.setStatus(VendorStatus.ACTIVE.getStatusValue());
			// to do add payment this to payment history table for record
			vendorRepository.save(vendor);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("vendor.subscription.plan.not", null));
		}
	}

	@Override
	public void updateRestaurantDetails(final VendorRestaurantDetailsDTO vendorRestaurantDetailsDTO, final MultipartFile storeImage,
			final MultipartFile storeDetailImage, final MultipartFile featuredImage) throws NotFoundException, ValidationException {
		Vendor vendor = getVendorDetail(vendorRestaurantDetailsDTO.getVendorId());
		if (vendor.getActive().booleanValue() && VendorStatus.ACTIVE.getStatusValue().equals(vendor.getStatus())) {
			BeanUtils.copyProperties(vendorRestaurantDetailsDTO, vendor);
			if (VendorAccepts.getByValue(vendorRestaurantDetailsDTO.getAccepts()) == null) {
				throw new ValidationException(messageByLocaleService.getMessage("invalid.accepts", null));
			} else if (PaymentMethod.getByValue(vendorRestaurantDetailsDTO.getPaymentMethod()) == null) {
				throw new ValidationException(messageByLocaleService.getMessage("invalid.payment.method", null));
			} else if (DeliveryType.getByValue(vendorRestaurantDetailsDTO.getDeliveryType()) == null) {
				throw new ValidationException(messageByLocaleService.getMessage("invalid.delivery.type", null));
			}
			/**
			 * if order service is enable by vendor then check he has active subscription plan or not
			 */
			else if (vendor.getIsOrderServiceEnable().booleanValue()
					&& (vendor.getSubscriptionPlan() == null || VendorStatus.EXPIRED.getStatusValue().equals(vendor.getStatus()))) {
				throw new ValidationException(messageByLocaleService.getMessage("purchase.subscriptionPlan", null));
			} else if (vendor.getIsOrderServiceEnable().booleanValue() && (VendorStatus.SUSPENDED.getStatusValue().equals(vendor.getStatus()))) {
				throw new ValidationException(messageByLocaleService.getMessage("suspended.vendor", null));
			}

			if (storeImage != null && CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(storeImage.getOriginalFilename())) {
				if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(vendor.getStoreImageName())) {
					assetService.deleteFile(vendor.getStoreImageName(), AssetConstant.VENDOR);
				}
				vendor.setStoreImageName(assetService.saveAsset(storeImage, AssetConstant.VENDOR, 0));
				vendor.setStoreImageOriginalName(storeImage.getOriginalFilename());
			}
			if (storeDetailImage != null && CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(storeDetailImage.getOriginalFilename())) {
				if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(vendor.getStoreDetailImageName())) {
					assetService.deleteFile(vendor.getStoreDetailImageName(), AssetConstant.VENDOR);
				}
				vendor.setStoreDetailImageName(assetService.saveAsset(storeDetailImage, AssetConstant.VENDOR, 1));
				vendor.setStoreDetailImageOriginalName(storeDetailImage.getOriginalFilename());
			}
			if (featuredImage != null && CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(featuredImage.getOriginalFilename())) {
				if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(vendor.getFeaturedImageName())) {
					assetService.deleteFile(vendor.getFeaturedImageName(), AssetConstant.VENDOR);
				}
				vendor.setFeaturedImageName(assetService.saveAsset(featuredImage, AssetConstant.VENDOR, 2));
				vendor.setFeaturedImageOriginalName(featuredImage.getOriginalFilename());
			}
			vendorRepository.save(vendor);
			/**
			 * set vendor cuisine
			 */
			if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(vendorRestaurantDetailsDTO.getCuisineIds())) {
				List<VendorCuisine> existingVendorCuisines = vendorCuisineService.getVendorCuisineListByVendor(vendor.getId(), true);
				List<Long> existingCuisines = existingVendorCuisines.stream().map(x -> x.getCuisine().getId()).collect(Collectors.toList());
				List<Long> newCuisines = new ArrayList<>(vendorRestaurantDetailsDTO.getCuisineIds());
				/**
				 * Not existing variant (new) that to be added
				 */
				newCuisines.removeAll(existingCuisines);

				/**
				 * Delete existing which are not in request
				 */
				existingCuisines.removeAll(vendorRestaurantDetailsDTO.getCuisineIds());

				/**
				 * Based On deleted sizeId find out deleted variant.
				 */
				List<VendorCuisine> deletedVendorCuisines = existingVendorCuisines.stream().filter(x -> existingCuisines.contains(x.getCuisine().getId()))
						.collect(Collectors.toList());
				vendorCuisineService.bulkChangeAllStatus(deletedVendorCuisines, false);
				for (Long cuisineId : newCuisines) {
					VendorCuisineDTO vendorCuisineDTO = new VendorCuisineDTO();
					vendorCuisineDTO.setActive(true);
					vendorCuisineDTO.setCuisineId(cuisineId);
					vendorCuisineDTO.setVendorId(vendor.getId());
					vendorCuisineService.addUpdateVendorCuisine(vendorCuisineDTO);
				}
			}
		} else {
			throw new ValidationException(messageByLocaleService.getMessage(VENDOR_ACTIVE_FIRST, null));
		}
	}

	@Override
	public void updateOrderServiceEnableForVendor(final Long vendorId, final Boolean isOrderServiceEnable) throws ValidationException, NotFoundException {
		Vendor vendor = getVendorDetail(vendorId);

		if (isOrderServiceEnable == null) {
			throw new ValidationException(messageByLocaleService.getMessage("isOrderServiceEnable.not.null", null));
		} else if (vendor.getSubscriptionPlan() == null || VendorStatus.EXPIRED.getStatusValue().equals(vendor.getStatus())) {
			throw new ValidationException(messageByLocaleService.getMessage("purchase.subscriptionPlan", null));
		} else if (VendorStatus.SUSPENDED.getStatusValue().equals(vendor.getStatus())) {
			throw new ValidationException(messageByLocaleService.getMessage("suspended.vendor", null));
		} else if (vendor.getIsOrderServiceEnable().equals(isOrderServiceEnable)) {
			throw new ValidationException(
					messageByLocaleService.getMessage(Boolean.TRUE.equals(isOrderServiceEnable) ? "already.enable" : "already.disable", null));
		}
		vendor.setIsOrderServiceEnable(isOrderServiceEnable);
		vendorRepository.save(vendor);
	}

	@Override
	public Long getVendorCountBasedOnParams(final VendorFilterDTO vendorFilterDTO) {
		return vendorRepository.getVendorCountBasedOnParams(vendorFilterDTO);
	}

	@Override
	public List<Vendor> getVendorListBasedOnParams(final Integer startIndex, final Integer pageSize, final VendorFilterDTO vendorFilterDTO)
			throws ValidationException {
		return vendorRepository.getVendorListBasedOnParams(startIndex, pageSize, vendorFilterDTO);
	}

	@Override
	public List<VendorResponseDTO> getVendorListForApp(final VendorListFilterDTO vendorListFilterDTO) throws ValidationException, NotFoundException {
		if (vendorListFilterDTO.getCustomerAddressId() == null) {
			if (vendorListFilterDTO.getLatitude() == null || vendorListFilterDTO.getLongitude() == null) {
				throw new ValidationException(messageByLocaleService.getMessage("location.address.required", null));
			}
		} else {
			CustomerAddress customerAddress = customerAddressService.getAddressDetails(vendorListFilterDTO.getCustomerAddressId());
			vendorListFilterDTO.setLatitude(customerAddress.getLatitude());
			vendorListFilterDTO.setLongitude(customerAddress.getLongitude());
		}
		List<VendorResponseDTO> responseDTOs = new ArrayList<>();
		List<Vendor> vendors = vendorRepository.getVendorListForCustomerBasedOnParams(null, null, vendorListFilterDTO);
		int size = vendors.size();

		for (int i = 0; i < size; i++) {
			Double min = null;
			Vendor resultVendor = null;
			for (Vendor vendor : vendors) {
				Double distance = CommonUtility.distance(vendor.getLatitude().doubleValue(), vendor.getLongitude().doubleValue(),
						vendorListFilterDTO.getLatitude().doubleValue(), vendorListFilterDTO.getLongitude().doubleValue());
				if (distance <= Constant.MAX_DISTANCE_FROM_CUSTOMER) {
					if (min == null) {
						min = distance;
						resultVendor = vendor;
					} else {
						if (distance < min) {
							min = distance;
							resultVendor = vendor;
						}
					}
				}
			}
			if (resultVendor != null) {
				VendorResponseDTO vendorResponseDTO = vendorMapper.toDto(resultVendor);
				vendorResponseDTO.setDistance(CommonUtility.distance(resultVendor.getLatitude().doubleValue(), resultVendor.getLongitude().doubleValue(),
						vendorListFilterDTO.getLatitude().doubleValue(), vendorListFilterDTO.getLongitude().doubleValue()));
				responseDTOs.add(vendorResponseDTO);
				vendors.remove(resultVendor);
			}
		}
		return responseDTOs;
	}

	@Override
	public Boolean isVendorContactExists(final VendorDTO vendorDTO) {
		if (vendorDTO.getId() != null) {
			/**
			 * At the time of update is vendor with same contact exist or not except it's own id
			 */
			return vendorRepository.findByPhoneNumberAndIdNot(vendorDTO.getPhoneNumber(), vendorDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is vendor with same contact exist or not
			 */
			Optional<Vendor> optVendor = vendorRepository.findByPhoneNumber(vendorDTO.getPhoneNumber());
			if (optVendor.isPresent()) {
				return !vendorDTO.getEmail().equalsIgnoreCase(optVendor.get().getEmail());
			} else {
				return false;
			}
		}
	}

	@Override
	public void runVendorSubscriptionExpireScheduler(final Date runDate) {
		VendorFilterDTO vendorFilterDTO = new VendorFilterDTO();
		vendorFilterDTO.setSubscriptionEndDate(runDate);
		List<Vendor> vendors = vendorRepository.getVendorListBasedOnParams(null, null, vendorFilterDTO);
		for (Vendor vendor : vendors) {
			vendor.setActive(false);
			vendor.setStatus(VendorStatus.EXPIRED.name());
			vendor.setIsOrderServiceEnable(false);
			vendorRepository.save(vendor);

		}

	}

	@Override
	public void runVendorSubscriptionExpireReminderScheduler(final Date runDate) {
		VendorFilterDTO vendorFilterDTO = new VendorFilterDTO();
		LocalDate localDate = CommonUtility.convetUtilDatetoLocalDate(runDate);
		localDate = localDate.plusDays(7L);
		vendorFilterDTO.setSubscriptionEndDate(CommonUtility.convertLocalDateToUtilDate(localDate));
		List<Vendor> vendors = vendorRepository.getVendorListBasedOnParams(null, null, vendorFilterDTO);
		for (Vendor vendor : vendors) {
			Notification notification = new Notification();
			notification.setEmail(vendor.getEmail());
			notification.setVendorId(vendor.getId());
			notification.setUserType(UserType.VENDOR.name());
			notification.setType(NotificationQueueConstants.VENDOR_SUBSCRIPTION_EXPIRY_REMINDER);
			jmsQueuerService.sendEmail(NotificationQueueConstants.GENERAL_QUEUE, notification);
		}

	}

	@Override
	public String changeVendorStatus(final Long vendorId, final String newStatus) throws NotFoundException, ValidationException {
		Vendor vendor = getVendorDetail(vendorId);
		VendorStatus existingStatus = VendorStatus.getByValue(vendor.getStatus());
		if (!existingStatus.contains(newStatus)) {
			throw new ValidationException(messageByLocaleService.getMessage("vendor.status.not.allowed", new Object[] { newStatus, vendor.getStatus() }));
		}
		if (VendorStatus.APPROVED.getStatusValue().equals(newStatus)) {
			// send email for you account has been approved by admin kindly Login here
		}
		if (VendorStatus.REJECTED.getStatusValue().equals(newStatus)) {
			// send email for you account has been rejected by admin kindly contact admin
		}
		if (VendorStatus.SUSPENDED.getStatusValue().equals(newStatus)) {
			// send email to vendor that your account is being suspended by admin kindly
			// contact admin.
			// revoke token
		}
		if (VendorStatus.EXPIRED.getStatusValue().equals(newStatus)
				|| (VendorStatus.ACTIVE.getStatusValue().equals(newStatus) && !VendorStatus.SUSPENDED.getStatusValue().equals(vendor.getStatus()))) {
			throw new ValidationException(messageByLocaleService.getMessage("status.not.allowed.here", new Object[] { newStatus }));
		}
		vendor.setStatus(newStatus);
		vendorRepository.save(vendor);
		Optional<UserLogin> userLogin = userLoginService.getUserLoginBasedOnEmailAndRole(vendor.getEmail(), Role.VENDOR.name());
		if (userLogin.isPresent()) {
			return userLogin.get().getEmail();
		} else {
			throw new NotFoundException(messageByLocaleService.getMessage("user.not.exists.email", new Object[] { vendor.getEmail() }));
		}
	}

	@Override
	public void exportVendorList(final VendorFilterDTO vendorFilterDTO, final HttpServletResponse httpServletResponse) throws FileNotFoundException {
		List<Vendor> vendorList;
		List<VendorExport> vendorExportList = new ArrayList<>();
		vendorList = vendorRepository.getVendorListBasedOnParams(null, null, vendorFilterDTO);
		for (Vendor vendor : vendorList) {
			final VendorExport vendorExport = new VendorExport();
			BeanUtils.copyProperties(vendor, vendorExport);
			vendorExportList.add(vendorExport);
		}
		final Object[] vendorHeaderField = new Object[] { "First Name", "Last Name", "Email", "Store Name", "Phone Number" };
		final Object[] vendorDataField = new Object[] { "firstName", "lastName", "email", "storeName", "phoneNumber" };
		try {
			exportCSV.writeCSVFile(vendorExportList, vendorDataField, vendorHeaderField, httpServletResponse);
		} catch (IOException e) {
			throw new FileNotFoundException(messageByLocaleService.getMessage("export.file.create.error", null));
		}

	}

	@Override
	public void verifyVendorContact(final Long vendorId, final String otp) throws NotFoundException, ValidationException {
		Vendor vendor = getVendorDetail(vendorId);
		if (VendorStatus.ACTIVE.getStatusValue().equals(vendor.getStatus()) && vendor.getActive().booleanValue()) {
			UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
			otpService.verifyOtp(userLogin.getId(), UserOtpTypeEnum.SMS.name(), otp, false);
			vendor.setPhoneVerified(true);
			vendorRepository.save(vendor);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage(VENDOR_ACTIVE_FIRST, null));
		}
	}

	@Override
	public void deleteVendorImageByType(final Long vendorId, final String type) throws NotFoundException, ValidationException {
		Vendor vendor = getVendorDetail(vendorId);
		String imageName = null;
		if (type.equals(Constant.VENDOR_FEATURED_IMAGE)) {
			imageName = vendor.getFeaturedImageName();
			vendor.setFeaturedImageName(null);
			vendor.setFeaturedImageOriginalName(null);
		} else if (type.equals(Constant.VENDOR_STORE_DETAIL_IMAGE)) {
			imageName = vendor.getStoreDetailImageName();
			vendor.setStoreDetailImageName(null);
			vendor.setStoreDetailImageOriginalName(null);
		} else if (type.equals(Constant.VENDOR_STORE_IMAGE)) {
			imageName = vendor.getStoreImageName();
			vendor.setStoreImageName(null);
			vendor.setStoreImageOriginalName(null);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.image.type", null));
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(imageName)) {
			assetService.deleteFile(imageName, AssetConstant.VENDOR);
		}
		vendorRepository.save(vendor);
	}

	@Override
	public void changeStatusOfIsFeaturedVendor(final Long vendorId, final Boolean active) throws NotFoundException, ValidationException {
		Vendor vendor = getVendorDetail(vendorId);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (vendor.getIsFeatured().equals(active)) {
			throw new ValidationException(
					messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "vendor.already.featured" : "vendor.already.not.featured", null));
		} else {
			vendor.setIsFeatured(active);
			vendorRepository.save(vendor);
		}
	}

	@Override
	public VendorBasicDetailDTO getVendorBasicDetailById(final Long vendorId) throws NotFoundException {
		return vendorMapper.toBasicDto(getVendorDetail(vendorId));
	}
}
