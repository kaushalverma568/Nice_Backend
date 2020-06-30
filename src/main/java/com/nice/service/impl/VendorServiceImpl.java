package com.nice.service.impl;

import java.time.ZoneId;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.nice.constant.AssetConstant;
import com.nice.constant.DeliveryType;
import com.nice.constant.NotificationQueueConstants;
import com.nice.constant.PaymentMethod;
import com.nice.constant.Role;
import com.nice.constant.UserOtpTypeEnum;
import com.nice.constant.UserType;
import com.nice.constant.VendorAccepts;
import com.nice.constant.VendorStatus;
import com.nice.dto.Notification;
import com.nice.dto.UserOtpDto;
import com.nice.dto.VendorBankDetailsDTO;
import com.nice.dto.VendorDTO;
import com.nice.dto.VendorFilterDTO;
import com.nice.dto.VendorResponseDTO;
import com.nice.dto.VendorRestaurantDetailsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.jms.queue.JMSQueuerService;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.VendorMapper;
import com.nice.model.BusinessCategory;
import com.nice.model.City;
import com.nice.model.Country;
import com.nice.model.Pincode;
import com.nice.model.SubscriptionPlan;
import com.nice.model.UserLogin;
import com.nice.model.UserOtp;
import com.nice.model.Vendor;
import com.nice.model.VendorBankDetails;
import com.nice.repository.VendorBankDetailsRepository;
import com.nice.repository.VendorRepository;
import com.nice.service.AssetService;
import com.nice.service.BusinessCategoryService;
import com.nice.service.CityService;
import com.nice.service.CountryService;
import com.nice.service.FileStorageService;
import com.nice.service.OtpService;
import com.nice.service.PincodeService;
import com.nice.service.SubscriptionPlanService;
import com.nice.service.UserLoginService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 24-Mar-2020
 * @description :
 */

@Transactional(rollbackFor = Throwable.class)
@Service("vendorService")
public class VendorServiceImpl implements VendorService {

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
	private AssetService assetService;

	@Autowired
	private OtpService otpService;

	@Autowired
	private FileStorageService fileStorageService;

	@Autowired
	private JMSQueuerService jmsQueuerService;

	@Override
	public void addVendor(final VendorDTO vendorDTO, final MultipartFile profilePicture) throws ValidationException, NotFoundException {
		if (!CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendorDTO.getPassword())) {
			throw new ValidationException(messageByLocaleService.getMessage("password.required", null));
		}
		Vendor vendor = vendorMapper.toEntity(vendorDTO);
		BusinessCategory businessCategory = businessCategoryService.getBusinessCategoryDetail(vendorDTO.getBusinessCategoryId());
		Country country = countryService.getCountryDetails(vendorDTO.getCountryId());
		City city = cityService.getCityDetails(vendorDTO.getCityId());
		Pincode pincode = pincodeService.getPincodeDetails(vendorDTO.getPincodeId());
		if (profilePicture != null) {
			uploadImage(profilePicture, vendor);
		}
		/**
		 * at the time of creation status will be verification pending
		 */
		vendor.setStatus(VendorStatus.VERIFICATION_PENDING.getStatusValue());
		vendor.setIsEmailVerified(false);
		vendor.setActive(false);
		vendor.setIsOrderServiceEnable(false);
		vendor.setBusinessCategory(businessCategory);
		vendor.setCity(city);
		vendor.setCountry(country);
		vendor.setPincode(pincode);
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
	public void updateBankDetails(final VendorBankDetailsDTO vendorBankDetailsDTO) throws NotFoundException {
		Vendor vendor = getVendorDetail(vendorBankDetailsDTO.getVendorId());
		Optional<VendorBankDetails> existingVendorBankDetails = vendorBankDetailsRepository.findByVendor(vendor);
		VendorBankDetails vendorBankDetails = new VendorBankDetails();
		BeanUtils.copyProperties(vendorBankDetailsDTO, vendorBankDetails);
		if (existingVendorBankDetails.isPresent()) {
			vendorBankDetails.setId(vendorBankDetails.getId());
		}
		vendorBankDetails.setVendor(vendor);
		vendorBankDetails.setActive(true);
		vendorBankDetailsRepository.save(vendorBankDetails);
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
				return vendorRepository.findAllByActiveAndIsEmailVerified(activeRecords, isEmailVerified, pageable);
			} else {
				return vendorRepository.findAllByIsEmailVerified(isEmailVerified, pageable);
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
				 * if vendor has active orders and not delivered yet then can't deactive
				 */
			} else {
				if (!vendor.getIsEmailVerified().booleanValue()) {
					throw new ValidationException(messageByLocaleService.getMessage("email.not.verified", null));
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
	public void uploadProfilePicture(final MultipartFile profilePicture, final Long vendorId) throws NotFoundException, ValidationException {
		Vendor vendor = getVendorDetail(vendorId);
		deleteOldImage(vendor);
		uploadImage(profilePicture, vendor);
		vendorRepository.save(vendor);
	}

	@Override
	public void deleteProfilePicture(final Long vendorId) throws NotFoundException {
		Vendor vendor = getVendorDetail(vendorId);
		if (!CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendor.getProfilePictureName())) {
			throw new NotFoundException(messageByLocaleService.getMessage("profile.image.not.found", null));
		} else {
			deleteOldImage(vendor);
			vendor.setProfilePictureName(null);
			vendor.setProfilePictureOriginalName(null);
			vendorRepository.save(vendor);
		}
	}

	@Override
	public Boolean isVendorExists(final VendorDTO vendorDTO) {
		if (vendorDTO.getId() != null) {
			/**
			 * At the time of update is vendor with same email exist or not except it's own
			 * id
			 */
			return vendorRepository.findByEmailAndIdNot(vendorDTO.getEmail(), vendorDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is vendor with same email exist or not
			 */
			return vendorRepository.findByEmail(vendorDTO.getEmail()).isPresent();
		}
	}

	/**
	 * upload profile picture of vendor
	 *
	 * @param profilePicture
	 * @param vendor
	 */
	private void uploadImage(final MultipartFile profilePicture, final Vendor vendor) {
		vendor.setProfilePictureName(assetService.saveAsset(profilePicture, AssetConstant.VENDOR, 0));
		vendor.setProfilePictureOriginalName(profilePicture.getOriginalFilename());
	}

	@Override
	public Boolean isUserLoginExists(final VendorDTO vendorDTO) {
		Optional<UserLogin> optUserLogin = userLoginService.getUserLoginBasedOnEmailAndRole(vendorDTO.getEmail(), Role.VENDOR.name());
		if (optUserLogin.isPresent()) {
			if (vendorDTO.getId() != null && vendorDTO.getId().equals(optUserLogin.get().getEntityId())) {
				return false;
			} else {
				return true;
			}
		} else {
			return false;
		}
	}

	@Override
	public void verifyEmail(final Long vendorId) throws NotFoundException {
		Vendor vendor = getVendorDetail(vendorId);
		/**
		 * if vendor is verifying his email for first time then his old status will
		 * verification pending
		 */
		if (VendorStatus.VERIFICATION_PENDING.getStatusValue().equals(vendor.getStatus())) {
			vendor.setStatus(VendorStatus.NEW.getStatusValue());
		}
		vendor.setIsEmailVerified(true);
		vendorRepository.save(vendor);
	}

	/**
	 * @param userLogin
	 * @param vendor
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
		notification.setType(NotificationQueueConstants.EMAIL_VERIFICATION);
		jmsQueuerService.sendEmail(NotificationQueueConstants.NON_NOTIFICATION_QUEUE, notification);
	}

	/**
	 * delete old profile picture
	 *
	 * @param vendor
	 */
	private void deleteOldImage(final Vendor vendor) {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendor.getProfilePictureName())) {
			fileStorageService.deleteFile(vendor.getProfilePictureName(), AssetConstant.VENDOR);
		}
	}

	@Override
	public String updatePersonalDetails(final VendorDTO vendorDTO, final MultipartFile profilePicture) throws NotFoundException, ValidationException {
		if (vendorDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("vendor.id.not.null", null));
		}
		Vendor existingVendor = getVendorDetail(vendorDTO.getId());
		Vendor vendor = vendorMapper.toEntity(vendorDTO);
		String userName = null;
		/**
		 * if vendor is updating his email then
		 */
		if (!existingVendor.getEmail().equals(vendorDTO.getEmail())) {
			/**
			 * if vendor has active orders then he can not update an email
			 */
			if (/* condition here */false) {
				throw new ValidationException(messageByLocaleService.getMessage("vendor.order.exist", null));
			} else {
				/**
				 * when vendor updates an email then de active him until email not verified and
				 * send verification link
				 */
				vendor.setIsEmailVerified(false);
				vendor.setIsOrderServiceEnable(false);
				vendor.setActive(false);

				final Optional<UserLogin> userLogin = userLoginService.getUserLoginBasedOnEmailAndRole(existingVendor.getEmail(), Role.VENDOR.name());
				if (userLogin.isPresent()) {
					userLogin.get().setEmail(vendorDTO.getEmail());
					userLogin.get().setActive(false);
					userLoginService.updateUserLogin(userLogin.get());
					userName = userLogin.get().getEmail();
					/**
					 * Code to generate OTP and send that in email.
					 */
					sendOtpForEmailVerification(userLogin.get(), vendor);
				}
			}
		} else {
			vendor.setIsEmailVerified(existingVendor.getIsEmailVerified());
			vendor.setIsOrderServiceEnable(existingVendor.getIsOrderServiceEnable());
			vendor.setActive(existingVendor.getActive());
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
		if (profilePicture != null) {
			deleteOldImage(existingVendor);
			uploadImage(profilePicture, vendor);
		} else {
			vendor.setProfilePictureName(existingVendor.getProfilePictureName());
			vendor.setProfilePictureOriginalName(existingVendor.getProfilePictureOriginalName());
		}
		vendorRepository.save(vendor);
		return userName;
	}

	@Override
	public void addUpdateSubscriptionPlan(final Long vendorId, final Long subscriptionPlanId) throws NotFoundException {
		SubscriptionPlan subscriptionPlan = subscriptionPlanService.getSubscriptionPlanDetail(subscriptionPlanId);
		Vendor vendor = getVendorDetail(vendorId);
		vendor.setSubscriptionPlanStartDate(new Date(System.currentTimeMillis()));
		/**
		 * add subscription duration days from subscription start date
		 */
		vendor.setSubscriptionPlanEndDate(Date.from(CommonUtility.convetUtilDatetoLocalDate(vendor.getSubscriptionPlanStartDate())
				.plusDays(subscriptionPlan.getDays()).atStartOfDay().atZone(ZoneId.systemDefault()).toInstant()));
		vendorRepository.save(vendor);
	}

	@Override
	public void updateRestaurantDetails(final VendorRestaurantDetailsDTO vendorRestaurantDetailsDTO) throws NotFoundException, ValidationException {
		Vendor vendor = getVendorDetail(vendorRestaurantDetailsDTO.getVendorId());
		BeanUtils.copyProperties(vendorRestaurantDetailsDTO, vendor);
		if (VendorAccepts.getByValue(vendorRestaurantDetailsDTO.getAccepts()) == null) {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.accepts", null));
		} else if (PaymentMethod.getByValue(vendorRestaurantDetailsDTO.getPaymentMethod()) == null) {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.payment.method", null));
		} else if (DeliveryType.getByValue(vendorRestaurantDetailsDTO.getDeliveryType()) == null) {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.delivery.type", null));
		}
		vendorRepository.save(vendor);
	}

	@Override
	public void updateOrderServiceEnableForVendor(final Long vendorId, final Boolean isOrderServiceEnable) throws ValidationException, NotFoundException {
		Vendor vendor = getVendorDetail(vendorId);

		if (isOrderServiceEnable == null) {
			throw new ValidationException(messageByLocaleService.getMessage("isOrderServiceEnable.not.null", null));
		} else if (vendor.getSubscriptionPlan() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("purchase.subscriptionPlan", null));
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

}
