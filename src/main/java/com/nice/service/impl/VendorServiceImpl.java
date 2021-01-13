package com.nice.service.impl;

import java.io.IOException;
import java.lang.reflect.Field;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.mail.MessagingException;
import javax.servlet.http.HttpServletResponse;

import com.nice.config.ApplicationDataConfig;
import com.nice.dto.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.i18n.LocaleContextHolder;
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
import com.nice.constant.PaymentStatus;
import com.nice.constant.Role;
import com.nice.constant.SendingType;
import com.nice.constant.UserOtpTypeEnum;
import com.nice.constant.UserType;
import com.nice.constant.VendorAccepts;
import com.nice.constant.VendorStatus;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.jms.queue.JMSQueuerService;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.VendorMapper;
import com.nice.model.Addons;
import com.nice.model.Area;
import com.nice.model.BusinessCategory;
import com.nice.model.Category;
import com.nice.model.City;
import com.nice.model.Country;
import com.nice.model.CustomerAddress;
import com.nice.model.Product;
import com.nice.model.SubscriptionPlan;
import com.nice.model.UserLogin;
import com.nice.model.UserOtp;
import com.nice.model.Vendor;
import com.nice.model.VendorBankDetails;
import com.nice.model.VendorCuisine;
import com.nice.model.VendorHistory;
import com.nice.model.VendorPayment;
import com.nice.repository.UserOtpRepository;
import com.nice.repository.VendorBankDetailsRepository;
import com.nice.repository.VendorCuisineRepository;
import com.nice.repository.VendorHistoryRepository;
import com.nice.repository.VendorRepository;
import com.nice.service.AddonsService;
import com.nice.service.AreaService;
import com.nice.service.AssetService;
import com.nice.service.BusinessCategoryService;
import com.nice.service.CategoryService;
import com.nice.service.CityService;
import com.nice.service.CountryService;
import com.nice.service.CustomerAddressService;
import com.nice.service.HesabePaymentService;
import com.nice.service.OtpService;
import com.nice.service.ProductService;
import com.nice.service.RoleService;
import com.nice.service.SchedulerDetailsService;
import com.nice.service.SubscriptionPlanService;
import com.nice.service.UserLoginService;
import com.nice.service.VendorCuisineService;
import com.nice.service.VendorPaymentService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;
import com.nice.util.ExportCSV;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 24-Mar-2020
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
    private VendorPaymentService vendorPaymentService;

    @Autowired
    private CityService cityService;

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

    @Autowired
    private ProductService productService;

    @Autowired
    private AddonsService addonsService;

    @Autowired
    private CategoryService categoryService;

    @Autowired
    private HesabePaymentService hesabePaymentService;

    @Value("${service.url}")
    private String serviceUrl;

    @Autowired
    private RoleService roleService;

    @Autowired
    private AreaService areaService;

    @Autowired
    private VendorCuisineRepository vendorCuisineRepository;

    @Autowired
    private SchedulerDetailsService schedulerDetailsService;

    @Autowired
    private UserOtpRepository userOtpRepository;

    @Autowired
    private VendorHistoryRepository vendorHistoryRepository;

    @Autowired
    private ApplicationDataConfig appConfig;

    @Override
    public VendorResponseDTO addVendor(final VendorDTO vendorDTO, final MultipartFile storeImage, final MultipartFile featuredImage,
                                       final MultipartFile storeDetailImage) throws ValidationException, NotFoundException, FileOperationException {
        if (!CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendorDTO.getPassword())) {
            throw new ValidationException(messageByLocaleService.getMessage("password.required", null));
        }
        /**
         * Check if vendor already exists, if so then lets only send him email again.
         */
        Optional<Vendor> optVendor = vendorRepository.findByEmail(vendorDTO.getEmail().toLowerCase());
        if (!vendorDTO.getIsAdmin().booleanValue() && optVendor.isPresent() && !optVendor.get().getEmailVerified().booleanValue()) {
            Optional<UserLogin> optUserLogin = userLoginService.getUserLoginBasedOnEmailAndEntityType(optVendor.get().getEmail(), UserType.VENDOR.name());
            if (optUserLogin.isPresent()) {
                VendorResponseDTO vendorResponseDTO = vendorMapper.toDto(optVendor.get(), false);
                vendorResponseDTO.setUserId(optUserLogin.get().getId());
                return vendorResponseDTO;
            }
        }

        Vendor vendor = vendorMapper.toEntity(vendorDTO);
        /**
         * Set vendor preferred language to default language when vendor registers.
         */
        vendor.setPreferredLanguage(LocaleContextHolder.getLocale().getLanguage());

        BusinessCategory businessCategory = businessCategoryService.getBusinessCategoryDetail(vendorDTO.getBusinessCategoryId());
        Country country = countryService.getCountryDetails(vendorDTO.getCountryId());
        City city = cityService.getCityDetails(vendorDTO.getCityId());
        Area area = areaService.getAreaDetails(vendorDTO.getAreaId());
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
        vendor.setArea(area);
        vendor.setIsFeatured(false);
        vendor.setProfileCompleted(false);
        vendor.setNoOfRating(0L);
        vendor.setRating(0D);
        if (storeImage != null) {
            uploadImage(storeImage, vendor);
        }
        if (storeDetailImage != null) {
            uploadDetailImage(storeDetailImage, vendor);
        }
        if (featuredImage != null) {
            uploadFeaturedImage(featuredImage, vendor);
        }
        vendor = vendorRepository.save(vendor);
        addVendorHistory(vendor.getId());
        /**
         * set login details of vendor
         */
        final UserLogin userLogin = new UserLogin();
        userLogin.setEntityId(vendor.getId());
        userLogin.setEntityType(UserType.VENDOR.name());
        userLogin.setEmail(vendor.getEmail());
        userLogin.setRole(roleService.getRoleDetailByName(Role.VENDOR.getStatusValue()));
        userLogin.setPassword(vendorDTO.getPassword());
        userLogin.setActive(false);
        userLoginService.addUserLogin(userLogin);
        LOGGER.info("Inside add Vendor service vendor:{}", vendor);

        /**
         * Code to generate OTP and send that in email if vendor himself/herself signed
         * up.
         */
        if (vendorDTO.getIsAdmin().booleanValue()) {
            verifyEmailByAdmin(vendor.getId());
            changeVendorStatus(vendor.getId(), VendorStatus.APPROVED.getStatusValue());
        }
        VendorResponseDTO vendorResponseDTO = vendorMapper.toDto(vendor, false);
        vendorResponseDTO.setUserId(userLogin.getId());
        return vendorResponseDTO;
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
                addVendorHistory(vendor.getId());
            } else {
                throw new ValidationException(messageByLocaleService.getMessage(VENDOR_ACTIVE_FIRST, null));
            }
        } else {
            throw new ValidationException(messageByLocaleService.getMessage("vendor.update.restaurant.detail.first", null));
        }
    }

    @Override
    public VendorResponseDTO getVendor(final Long vendorId) throws NotFoundException {
        return vendorMapper.toDto(getVendorDetail(vendorId), true);
    }

    @Override
    public Vendor getVendorDetail(final Long vendorId) throws NotFoundException {
        return vendorRepository.findById(vendorId)
                .orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("vendor.not.found", new Object[]{vendorId})));
    }

    @Override
    public VendorBankDetails getVendorBankDetails(final Long vendorId) throws NotFoundException {
        return vendorBankDetailsRepository.findByVendor(getVendorDetail(vendorId))
                .orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("vendor.bank.details.found", new Object[]{vendorId})));
    }

    @Override
    public Page<Vendor> getVendorList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final Boolean isEmailVerified)
            throws NotFoundException {
        Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("firstName"));
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
        final Optional<UserLogin> userLogin = userLoginService.getUserLoginBasedOnEmailAndEntityType(vendor.getEmail(), UserType.VENDOR.name());
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
                 * deactivate all product of this vendor
                 */
                ProductParamRequestDTO productParamRequestDTO = new ProductParamRequestDTO();
                productParamRequestDTO.setVendorId(vendorId);
                List<Product> products = productService.getProductListBasedOnParamsWithoutPagination(productParamRequestDTO);
                for (Product product : products) {
                    productService.changeStatus(product.getId(), active);
                }
                /**
                 * deactivate all addons for this vendor
                 */
                List<Addons> addons = addonsService.getAddonsListByVendor(vendorId);
                for (Addons addons2 : addons) {
                    addonsService.changeStatus(addons2.getId(), active);
                }
                /**
                 * deactivate all category for this vendor
                 */
                List<Category> categories = categoryService.getCategoryListByVendor(vendorId);
                for (Category category : categories) {
                    categoryService.changeStatus(category.getId(), active);
                }
                /**
                 * deActive all vendor cuisines of this vendor
                 */
                List<VendorCuisine> vendorCuisineList = vendorCuisineService.getVendorCuisineListByVendor(vendorId, true);
                for (VendorCuisine vendorCuisine : vendorCuisineList) {
                    vendorCuisineService.changeStatus(vendorCuisine.getId(), false);
                }
                userName = userLogin.get().getEmail();
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
            addVendorHistory(vendor.getId());
        } else {
            throw new NotFoundException(messageByLocaleService.getMessage("user.not.exists.email", new Object[]{vendor.getEmail()}));
        }
        return userName;
    }

    @Override
    public Boolean isVendorExists(final VendorDTO vendorDTO) {
        if (vendorDTO.getId() != null) {
            /**
             * At the time of update is vendor with same email exist or not except it's own
             * id
             */
            return vendorRepository.findByEmailAndIdNot(vendorDTO.getEmail().toLowerCase(), vendorDTO.getId()).isPresent();
        } else {
            /**
             * At the time of create is vendor with same email exist or not
             */
            Optional<Vendor> vendor = vendorRepository.findByEmail(vendorDTO.getEmail().toLowerCase());
            if (vendor.isPresent()) {
                /**
                 * If the vendor is present and his email not verified, then we will be sending
                 * the verification link for him again, if the email is verified then we will be
                 * returning true.
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
             * If the vendor is present and his email not verified, then we will be sending
             * the verification link for him again, if the email is verified then we will be
             * returning true.
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
            return vendorDTO.getId() == null || !UserType.VENDOR.name().equals(optUserLogin.get().getEntityType())
                    || !vendorDTO.getId().equals(optUserLogin.get().getEntityId());
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
        vendor.setEmailVerified(true);
        vendor.setActive(true);
        vendorRepository.save(vendor);
        addVendorHistory(vendor.getId());
    }

    /**
     * @param userLogin
     * @param vendor
     * @throws NotFoundException
     * @throws ValidationException
     * @throws MessagingException
     */
    @Override
    public void sendOtpForEmailVerification(final VendorResponseDTO vendorResponseDTO) throws NotFoundException, ValidationException {
        UserOtpDto userOtpDto = new UserOtpDto();
        userOtpDto.setEmail(vendorResponseDTO.getEmail());
        userOtpDto.setType(UserOtpTypeEnum.EMAIL.name());
        userOtpDto.setUserId(vendorResponseDTO.getUserId());
        UserOtp otp = otpService.generateOtp(userOtpDto);
        sendEmail(otp.getOtp(), vendorResponseDTO);
    }

    private void sendEmail(final String otp, final VendorResponseDTO vendorResponseDTO) {
        Notification notification = new Notification();
        notification.setOtp(otp);
        notification.setUserId(vendorResponseDTO.getUserId());
        notification.setEmail(vendorResponseDTO.getEmail());
        notification.setVendorId(vendorResponseDTO.getId());
        /**
         * we will send verification link only
         */
        notification.setSendingType(SendingType.LINK.name());
        notification.setUserType(UserType.VENDOR.name());
        notification.setType(NotificationQueueConstants.EMAIL_VERIFICATION);
        notification.setLanguage(vendorResponseDTO.getPreferredLanguage());
        jmsQueuerService.sendEmail(NotificationQueueConstants.NON_NOTIFICATION_QUEUE, notification);
    }

    @Override
    public void updatePersonalDetails(final VendorDTO vendorDTO) throws NotFoundException, ValidationException {
        if (vendorDTO.getId() == null) {
            throw new ValidationException(messageByLocaleService.getMessage("vendor.id.not.null", null));
        } else if (!CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(vendorDTO.getPreferredLanguage())) {
            throw new ValidationException(messageByLocaleService.getMessage("preferred.language.not.null", null));
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
            Area area = areaService.getAreaDetails(vendorDTO.getAreaId());
            vendor.setBusinessCategory(businessCategory);
            vendor.setCity(city);
            vendor.setCountry(country);
            vendor.setArea(area);
            vendor.setRating(existingVendor.getRating());
            vendor.setNoOfRating(existingVendor.getNoOfRating());
            vendor.setSubscriptionPlan(existingVendor.getSubscriptionPlan());
            vendor.setSubscriptionPlanStartDate(existingVendor.getSubscriptionPlanStartDate());
            vendor.setSubscriptionPlanEndDate(existingVendor.getSubscriptionPlanEndDate());
            vendor.setStatus(existingVendor.getStatus());
            vendor.setStoreDetailImageName(existingVendor.getStoreDetailImageName());
            vendor.setStoreImageName(existingVendor.getStoreImageName());
            vendor.setStoreDetailImageOriginalName(existingVendor.getStoreDetailImageOriginalName());
            vendor.setStoreImageOriginalName(existingVendor.getStoreImageOriginalName());
            vendor.setFeaturedImageName(existingVendor.getFeaturedImageName());
            vendor.setFeaturedImageOriginalName(existingVendor.getFeaturedImageOriginalName());
            vendor.setAccepts(existingVendor.getAccepts());
            vendor.setOpeningHoursFrom(existingVendor.getOpeningHoursFrom());
            vendor.setOpeningHoursTo(existingVendor.getOpeningHoursTo());
            vendor.setMinimumOrderAmt(existingVendor.getMinimumOrderAmt());
            vendor.setPaymentMethod(existingVendor.getPaymentMethod());
            vendor.setDeliveryType(existingVendor.getDeliveryType());
            vendor.setMaxDaysForAccept(existingVendor.getMaxDaysForAccept());
            vendor.setStorePhoneNumber(existingVendor.getStorePhoneNumber());
            vendor.setProfileCompleted(existingVendor.getProfileCompleted());
            vendor.setIsFeatured(existingVendor.getIsFeatured());
            vendorRepository.save(vendor);
            addVendorHistory(vendor.getId());
        } else {
            throw new ValidationException(messageByLocaleService.getMessage(VENDOR_ACTIVE_FIRST, null));
        }

    }

    @Override
    public String updateSubscriptionPlanForVendor(final Long vendorId, final Long subscriptionPlanId) throws NotFoundException, ValidationException {
        Vendor vendor = getVendorDetail(vendorId);
        if (VendorStatus.APPROVED.getStatusValue().equals(vendor.getStatus()) || VendorStatus.EXPIRED.getStatusValue().equals(vendor.getStatus())) {
            SubscriptionPlan subscriptionPlan = subscriptionPlanService.getSubscriptionPlanDetail(subscriptionPlanId);
            VendorPaymentDTO vendorPaymentDTO = new VendorPaymentDTO();
            vendorPaymentDTO.setStatus(PaymentStatus.PENDING.name());
            vendorPaymentDTO.setAmount(subscriptionPlan.getAmount());
            vendorPaymentDTO.setVendorOrderId(System.currentTimeMillis() + "_vendor_" + vendorId);
            vendorPaymentDTO.setSubscriptionPlanId(subscriptionPlanId);
            vendorPaymentDTO.setVendorId(vendorId);
            vendorPaymentDTO.setActive(true);
            String redirectUrl = serviceUrl + "vendor/subscription/hesabe";
            LOGGER.info("inside hesabe gateway for generate url");
            String url = hesabePaymentService.createPaymentGateway(vendorPaymentDTO.getVendorOrderId(), vendorPaymentDTO.getAmount(), redirectUrl);
            LOGGER.info("outside hesabe gateway for generate url");
            vendorPaymentService.addVendorPayment(vendorPaymentDTO);
            return url;
        } else {
            throw new ValidationException(messageByLocaleService.getMessage("vendor.subscription.plan.not", null));
        }
    }

    @Override
    public void updateRestaurantDetails(final VendorRestaurantDetailsDTO vendorRestaurantDetailsDTO, final MultipartFile storeImage,
                                        final MultipartFile storeDetailImage, final MultipartFile featuredImage) throws NotFoundException, ValidationException, FileOperationException {
        Vendor vendor = getVendorDetail(vendorRestaurantDetailsDTO.getVendorId());
        if (!vendor.getBusinessCategory().getManageInventory().booleanValue() && vendorRestaurantDetailsDTO.getCuisineIds() == null) {
            throw new ValidationException(messageByLocaleService.getMessage("cuisine.ids.not.null", null));
        }
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
             * validation for pick-up delivery type
             */
            if (DeliveryType.PICKUP.getStatusValue().equals(vendorRestaurantDetailsDTO.getDeliveryType())
                    && (PaymentMethod.COD.getStatusValue().equals(vendorRestaurantDetailsDTO.getPaymentMethod())
                    || PaymentMethod.BOTH.getStatusValue().equals(vendorRestaurantDetailsDTO.getPaymentMethod()))) {
                throw new ValidationException(messageByLocaleService.getMessage("cod.pickup.both.not.allowed", null));
            }
            /**
             * if order service is enable by vendor then check he has active subscription
             * plan or not
             */
            else if (vendor.getIsOrderServiceEnable().booleanValue()
                    && (vendor.getSubscriptionPlan() == null || VendorStatus.EXPIRED.getStatusValue().equals(vendor.getStatus()))) {
                throw new ValidationException(messageByLocaleService.getMessage("purchase.subscriptionPlan", null));
            } else if (vendor.getIsOrderServiceEnable().booleanValue() && VendorStatus.SUSPENDED.getStatusValue().equals(vendor.getStatus())) {
                throw new ValidationException(messageByLocaleService.getMessage("suspended.vendor", null));
            }

            if (storeImage != null && CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(storeImage.getOriginalFilename())) {
                if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(vendor.getStoreImageName())) {
                    assetService.deleteFile(vendor.getStoreImageName(), AssetConstant.VENDOR);
                }
                vendor.setStoreImageName(
                        assetService.saveAsset(storeImage, AssetConstant.VENDOR, 0, Constant.VENDOR_LIST_IMAGE_WIDTH, Constant.VENDOR_LIST_IMAGE_HEIGHT));
                vendor.setStoreImageOriginalName(storeImage.getOriginalFilename());
            }
            if (storeDetailImage != null && CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(storeDetailImage.getOriginalFilename())) {
                if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(vendor.getStoreDetailImageName())) {
                    assetService.deleteFile(vendor.getStoreDetailImageName(), AssetConstant.VENDOR);
                }
                vendor.setStoreDetailImageName(assetService.saveAsset(storeDetailImage, AssetConstant.VENDOR, 1, Constant.VENDOR_DETAIL_IMAGE_WIDTH,
                        Constant.VENDOR_DETAIL_IMAGE_HEIGHT));
                vendor.setStoreDetailImageOriginalName(storeDetailImage.getOriginalFilename());
            }
            if (featuredImage != null && CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(featuredImage.getOriginalFilename())) {
                if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(vendor.getFeaturedImageName())) {
                    assetService.deleteFile(vendor.getFeaturedImageName(), AssetConstant.VENDOR);
                }
                vendor.setFeaturedImageName(assetService.saveAsset(featuredImage, AssetConstant.VENDOR, 2, Constant.VENDOR_FEATURE_IMAGE_WIDTH,
                        Constant.VENDOR_FEATURE_IMAGE_HEIGHT));
                vendor.setFeaturedImageOriginalName(featuredImage.getOriginalFilename());
            }
            vendorRepository.save(vendor);
            addVendorHistory(vendor.getId());
            /**
             * set vendor cuisine
             */
            if (!vendor.getBusinessCategory().getManageInventory().booleanValue() && vendorRestaurantDetailsDTO.getCuisineIds() != null) {
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
                    Optional<VendorCuisine> vendorCuisine = vendorCuisineRepository.findAllByVendorIdAndCuisineId(vendor.getId(), cuisineId);
                    if (vendorCuisine.isPresent()) {
                        vendorCuisineDTO.setId(vendorCuisine.get().getId());
                    }
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
        } else if (vendor.getProfileCompleted() != null && !vendor.getProfileCompleted().booleanValue()) {
            throw new ValidationException(messageByLocaleService.getMessage("vendor.profile.incomplete", null));
        }
        vendor.setIsOrderServiceEnable(isOrderServiceEnable);
        vendorRepository.save(vendor);
        addVendorHistory(vendor.getId());
    }

    @Override
    public Long getVendorCountBasedOnParams(final VendorFilterDTO vendorFilterDTO) {
        return vendorRepository.getVendorCountBasedOnParams(vendorFilterDTO);
    }

    @Override
    public List<Vendor> getVendorListBasedOnParams(final Integer startIndex, final Integer pageSize, final VendorFilterDTO vendorFilterDTO)
            throws ValidationException {
        UserLogin userLogin = getUserLoginFromToken();
        if (userLogin != null && UserType.VENDOR.name().equals(userLogin.getEntityType())) {
            throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
        }
        sortByFieldAndDirection(vendorFilterDTO);
        return vendorRepository.getVendorListBasedOnParams(startIndex, pageSize, vendorFilterDTO);
    }

    /**
     * @param sortByDirection
     * @param sortByField
     * @return
     * @throws ValidationException
     */
    private void sortByFieldAndDirection(final VendorFilterDTO vendorFilterDTO) throws ValidationException {
        validationForSortByFieldAndDirection(vendorFilterDTO);
        /**
         * Default Field is id
         */
        if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(vendorFilterDTO.getSortByField())) {
            if (!CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(vendorFilterDTO.getSortByDirection())) {
                vendorFilterDTO.setSortByDirection(Constant.SORT_DIRECTION_ASC);
            }
        } else {
            if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(vendorFilterDTO.getSortByDirection())) {
                vendorFilterDTO.setSortByField("id");
            } else {
                vendorFilterDTO.setSortByDirection(Constant.SORT_DIRECTION_DESC);
                vendorFilterDTO.setSortByField("id");
            }
        }
    }

    /**
     * @param sortByDirection
     * @param sortByField
     * @throws ValidationException
     */
    private void validationForSortByFieldAndDirection(final VendorFilterDTO vendorFilterDTO) throws ValidationException {
        if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(vendorFilterDTO.getSortByField())) {
            /**
             * Validate sortByField is valid field or not using reflection
             */
            Class<Vendor> vendorClass = Vendor.class;
            Field[] fields = vendorClass.getDeclaredFields();
            boolean isValid = false;
            for (Field field : fields) {
                if (vendorFilterDTO.getSortByField().equals(field.getName())) {
                    isValid = true;
                    break;
                }
            }
            if (!isValid) {
                throw new ValidationException(messageByLocaleService.getMessage("sort.field.invalid", null));
            }
        }
        if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(vendorFilterDTO.getSortByDirection())
                && !Constant.SORT_DIRECTION_ASC.equals(vendorFilterDTO.getSortByDirection())
                && !Constant.SORT_DIRECTION_DESC.equals(vendorFilterDTO.getSortByDirection())) {
            throw new ValidationException(messageByLocaleService.getMessage("sort.direction.invalid", null));
        }
    }

    @Override
    public List<VendorAppResponseDTO> getVendorListForApp(final VendorListFilterDTO vendorListFilterDTO, final Integer startIndex, final Integer pageSize)
            throws ValidationException, NotFoundException {
        /**
         * if customer address is not there then lat long should be there for
         * calculating distance.
         */
        if (vendorListFilterDTO.getCustomerAddressId() == null) {
            if (vendorListFilterDTO.getLatitude() == null || vendorListFilterDTO.getLongitude() == null) {
                throw new ValidationException(messageByLocaleService.getMessage("location.address.required", null));
            }
        } else {
            CustomerAddress customerAddress = customerAddressService.getAddressDetails(vendorListFilterDTO.getCustomerAddressId());
            vendorListFilterDTO.setLatitude(customerAddress.getLatitude());
            vendorListFilterDTO.setLongitude(customerAddress.getLongitude());
            vendorListFilterDTO.setCityId(customerAddress.getCity().getId());
            vendorListFilterDTO.setAreaId(customerAddress.getArea().getId());
        }
        /**
         * for sorting for popular and new arrival only one of them is allowed at time
         * if both then will throw exception.
         */
        if (vendorListFilterDTO.getIsPopular() != null && vendorListFilterDTO.getIsPopular().booleanValue() && vendorListFilterDTO.getIsNewArrival() != null
                && vendorListFilterDTO.getIsNewArrival().booleanValue()) {
            throw new ValidationException(messageByLocaleService.getMessage("only.one.sorting.allowed", null));
        }
        List<VendorAppResponseDTO> responseDTOs = new ArrayList<>();
        List<Vendor> vendors = vendorRepository.getVendorListForCustomerBasedOnParams(startIndex, pageSize, vendorListFilterDTO);
        for (Vendor vendor : vendors) {
            responseDTOs.add(vendorMapper.toAppDto(vendor, false));
        }
        return responseDTOs;
    }

    @Override
    public Long getVendorCountForCustomerBasedOnParams(final VendorListFilterDTO vendorListFilterDTO) throws ValidationException, NotFoundException {
        if (vendorListFilterDTO.getCustomerAddressId() == null) {
            if (vendorListFilterDTO.getLatitude() == null || vendorListFilterDTO.getLongitude() == null) {
                throw new ValidationException(messageByLocaleService.getMessage("location.address.required", null));
            }
        } else {
            CustomerAddress customerAddress = customerAddressService.getAddressDetails(vendorListFilterDTO.getCustomerAddressId());
            vendorListFilterDTO.setLatitude(customerAddress.getLatitude());
            vendorListFilterDTO.setLongitude(customerAddress.getLongitude());
            vendorListFilterDTO.setCityId(customerAddress.getCity().getId());
            vendorListFilterDTO.setAreaId(customerAddress.getArea().getId());
        }
        return vendorRepository.getVendorCountForCustomerBasedOnParams(vendorListFilterDTO);
    }

    @Override
    public Boolean isVendorContactExists(final VendorDTO vendorDTO) {
        if (vendorDTO.getId() != null) {
            /**
             * At the time of update is vendor with same contact exist or not except it's
             * own id
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
    public List<Long> runVendorSubscriptionExpireScheduler(final LocalDate runDate) {
        VendorFilterDTO vendorFilterDTO = new VendorFilterDTO();
        vendorFilterDTO.setSubscriptionEndDate(CommonUtility.convertLocalDateToUtilDate(runDate));
        List<Long> vendorIds = new ArrayList<>();
        List<Vendor> vendors = vendorRepository.getVendorListBasedOnParams(null, null, vendorFilterDTO);
        for (Vendor vendor : vendors) {
            vendor.setActive(false);
            vendor.setStatus(VendorStatus.EXPIRED.getStatusValue());
            vendor.setIsOrderServiceEnable(false);
            vendorRepository.save(vendor);
            addVendorHistory(vendor.getId());
            vendorIds.add(vendor.getId());
        }
        try {
            schedulerDetailsService.updateSchedulerDate(Constant.VENDOR_SUBSCRIPTION_EXPIRE, CommonUtility.convertLocalDateToUtilDate(runDate));
        } catch (NotFoundException e) {
            LOGGER.info("Error while executing scheduler ,{}", e.getMessage());
        }
        return vendorIds;
    }

    @Override
    public void runVendorSubscriptionExpireReminderScheduler(final LocalDate runDate) {
        VendorFilterDTO vendorFilterDTO = new VendorFilterDTO();
        LocalDate expireDate = runDate.plusDays(7L);
        vendorFilterDTO.setSubscriptionEndDate(CommonUtility.convertLocalDateToUtilDate(expireDate));
        List<Vendor> vendors = vendorRepository.getVendorListBasedOnParams(null, null, vendorFilterDTO);
        for (Vendor vendor : vendors) {
            Notification notification = new Notification();
            notification.setEmail(vendor.getEmail());
            notification.setVendorId(vendor.getId());
            notification.setUserType(UserType.VENDOR.name());
            notification.setType(NotificationQueueConstants.VENDOR_SUBSCRIPTION_EXPIRY_REMINDER);
            notification.setLanguage(vendor.getPreferredLanguage());
            jmsQueuerService.sendEmail(NotificationQueueConstants.GENERAL_QUEUE, notification);
        }
        try {
            schedulerDetailsService.updateSchedulerDate(Constant.VENDOR_SUBSCRIPTION_EXPIRE_REMINDER, CommonUtility.convertLocalDateToUtilDate(runDate));
        } catch (NotFoundException e) {
            LOGGER.info("Error while executing scheduler ,{}", e.getMessage());
        }

    }

    @Override
    public String changeVendorStatus(final Long vendorId, final String newStatus) throws NotFoundException, ValidationException {
        Vendor vendor = getVendorDetail(vendorId);
        VendorStatus existingStatus = VendorStatus.getByValue(vendor.getStatus());
        if (!existingStatus.contains(newStatus)) {
            throw new ValidationException(messageByLocaleService.getMessage("vendor.status.not.allowed", new Object[]{newStatus, vendor.getStatus()}));
        }
        if (VendorStatus.EXPIRED.getStatusValue().equals(newStatus)
                || VendorStatus.ACTIVE.getStatusValue().equals(newStatus) && !VendorStatus.SUSPENDED.getStatusValue().equals(vendor.getStatus())) {
            throw new ValidationException(messageByLocaleService.getMessage("status.not.allowed.here", new Object[]{newStatus}));
        }
        vendor.setStatus(newStatus);
        vendorRepository.save(vendor);
        addVendorHistory(vendor.getId());
        Optional<UserLogin> userLogin = userLoginService.getUserLoginBasedOnEmailAndEntityType(vendor.getEmail(), UserType.VENDOR.name());
        if (userLogin.isPresent()) {
            return userLogin.get().getEmail();
        } else {
            throw new NotFoundException(messageByLocaleService.getMessage("user.not.exists.email", new Object[]{vendor.getEmail()}));
        }
    }

    @Override
    public void exportVendorList(final VendorFilterDTO vendorFilterDTO, final HttpServletResponse httpServletResponse)
            throws FileNotFoundException, ValidationException {
        UserLogin userLogin = getUserLoginFromToken();
        if (userLogin != null && UserType.VENDOR.name().equals(userLogin.getEntityType())) {
            throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
        }
        List<Vendor> vendorList;
        List<VendorExport> vendorExportList = new ArrayList<>();
        vendorList = vendorRepository.getVendorListBasedOnParams(null, null, vendorFilterDTO);
        for (Vendor vendor : vendorList) {
            vendorExportList.add(vendorMapper.toExportDTO(vendor));
        }
        final Object[] vendorHeaderField = new Object[]{"First Name", "Last Name", "Email", "Store Name", "Phone Number", "Business Category Name",
                "is Featured", "status", "Return/Replace", "Store Phone Number", "preferred Language"};
        final Object[] vendorDataField = new Object[]{"firstName", "lastName", "email", "storeName", "phoneNumber", "businessCategoryName", "isFeatured",
                "status", "accepts", "storePhoneNumber", "preferredLanguage"};
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
            addVendorHistory(vendor.getId());
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
        addVendorHistory(vendor.getId());
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
            addVendorHistory(vendor.getId());
        }
    }

    @Override
    public VendorBasicDetailDTO getVendorBasicDetailById(final Long vendorId) throws NotFoundException {
        return vendorMapper.toBasicDto(getVendorDetail(vendorId));
    }

    @Override
    public void sendEmailForChangeVendorStatus(final Long vendorId) throws NotFoundException {
        Notification notification = new Notification();
        Vendor vendor = getVendorDetail(vendorId);
        notification.setVendorId(vendorId);
        notification.setType(NotificationQueueConstants.VENDOR_STATUS_CHANGE);
        notification.setLanguage(vendor.getPreferredLanguage());
        jmsQueuerService.sendEmail(NotificationQueueConstants.NON_NOTIFICATION_QUEUE, notification);
    }

    /**
     * for failed transaction
     *
     * @param vendorOrderId
     * @throws NotFoundException
     * @throws ValidationException
     */
    private void failedTransaction(final String vendorOrderId) throws NotFoundException, ValidationException {
        LOGGER.info("inside failed transaction");
        VendorPayment vendorPayment = vendorPaymentService.getVendorPaymentByVendorOrderIdAndStatus(vendorOrderId, PaymentStatus.PENDING.name());
        vendorPayment.setStatus(PaymentStatus.PAYMENT_FAIL.name());
        vendorPaymentService.updateVendorPayment(vendorPayment);
        LOGGER.info("outside failed transaction");
    }

    @Override
    public boolean checkPaymentTransactionHesabe(final HesabePaymentDTO hesabePaymentDTO) throws NotFoundException, ValidationException {
        boolean result = false;
        VendorPayment vendorPayment;
        vendorPayment = vendorPaymentService.getVendorPaymentByVendorOrderIdAndStatus(hesabePaymentDTO.getVariable1(), PaymentStatus.PENDING.name());

        if (!hesabePaymentDTO.getResultCode().equals(Constant.HESABE_CAPTURE)) {
            failedTransaction(hesabePaymentDTO.getVariable1());
            return result;
        }
        vendorPayment.setVendorOrderId(hesabePaymentDTO.getVariable1());
        vendorPayment.setPaymentToken(hesabePaymentDTO.getPaymentToken());
        vendorPayment.setPaymentId(hesabePaymentDTO.getPaymentId());
        vendorPayment.setAdministrativeCharge(hesabePaymentDTO.getAdministrativeCharge());

        addSubscriptionPlan(vendorPayment);

        result = true;
        vendorPayment.setStatus(PaymentStatus.PAYMENT_SUCCESS.name());
        vendorPaymentService.updateVendorPayment(vendorPayment);
        return result;
    }

    /**
     * for add vendor subscription
     *
     * @param vendorPayment
     * @throws ValidationException
     */
    private void addSubscriptionPlan(final VendorPayment vendorPayment) throws ValidationException {
        Vendor vendor = vendorPayment.getVendor();
        if (VendorStatus.APPROVED.getStatusValue().equals(vendor.getStatus())) {
            vendor.setActive(true);
        } else if (VendorStatus.EXPIRED.getStatusValue().equals(vendor.getStatus())) {
            vendor.setActive(true);
            vendor.setIsOrderServiceEnable(true);
        } else {
            throw new ValidationException(messageByLocaleService.getMessage("vendor.subscription.plan.not", null));
        }
        SubscriptionPlan subscriptionPlan = vendorPayment.getSubscriptionPlan();
        vendor.setSubscriptionPlanStartDate(new Date(System.currentTimeMillis()));
        vendor.setSubscriptionPlan(subscriptionPlan);
        /**
         * add subscription duration days from subscription start date
         */
        vendor.setSubscriptionPlanEndDate(Date.from(CommonUtility.convetUtilDatetoLocalDate(vendor.getSubscriptionPlanStartDate())
                .plusDays(subscriptionPlan.getDays()).atStartOfDay().atZone(ZoneId.systemDefault()).toInstant()));
        vendor.setStatus(VendorStatus.ACTIVE.getStatusValue());

        vendorRepository.save(vendor);
        addVendorHistory(vendor.getId());
    }

    private UserLogin getUserLoginFromToken() {
        Object principal = SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (Constant.ANONYMOUS_USER.equals(principal)) {
            return null;
        }
        return ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
    }

    @Override
    public VendorResponseDTO getVendorDetailForApp(final Long vendorId, final VendorListFilterDTO vendorListFilterDTO)
            throws ValidationException, NotFoundException {
        Vendor vendor = getVendorDetail(vendorId);
        /**
         * if customer address is not there then lat long should be there for
         * calculating distance.
         */
        if (vendorListFilterDTO.getCustomerAddressId() == null) {
            if (vendorListFilterDTO.getLatitude() == null || vendorListFilterDTO.getLongitude() == null) {
                throw new ValidationException(messageByLocaleService.getMessage("location.address.required", null));
            }
        } else {
            CustomerAddress customerAddress = customerAddressService.getAddressDetails(vendorListFilterDTO.getCustomerAddressId());
            vendorListFilterDTO.setLatitude(customerAddress.getLatitude());
            vendorListFilterDTO.setLongitude(customerAddress.getLongitude());
        }
        vendor.setDistance(
                vendorRepository.getVendorDistanceForCustomerBasedOnParams(vendorId, vendorListFilterDTO.getLatitude(), vendorListFilterDTO.getLongitude()));
        return vendorMapper.toDto(vendor, true);
    }

    @Override
    public Long getActiveVendor(final boolean active) {
        return vendorRepository.countByActive(active);
    }

    @Override
    public Long verifyEmailByAdmin(final Long vendorId) throws ValidationException, NotFoundException {
        /**
         * check if vendor is signed up by himself/herself then the OTP sent in email
         * should be expired.
         */
        UserLogin userLogin = userLoginService.getUserLoginBasedOnEntityIdAndEntityType(vendorId, UserType.VENDOR.name());
        Optional<UserOtp> userOtp = userOtpRepository.findAllByTypeIgnoreCaseAndUserLoginAndActive(UserOtpTypeEnum.EMAIL.name(), userLogin, true);
        if (userOtp.isPresent() && userOtp.get().getActive().booleanValue()) {
            userOtp.get().setActive(false);
            userOtpRepository.save(userOtp.get());
        }
        verifyEmail(vendorId);
        return userLogin.getId();
    }

    @Override
    public Long getAllVendorCount() {
        return vendorRepository.count();
    }

    @Override
    public void addVendorHistory(final Long vendorId) {
        Vendor vendor;
        try {
            vendor = getVendorDetail(vendorId);
            VendorHistory vendorHistory = new VendorHistory();
            BeanUtils.copyProperties(vendor, vendorHistory);
            vendorHistory.setId(null);
            vendorHistory.setVendor(vendor);
            vendorHistoryRepository.save(vendorHistory);
        } catch (NotFoundException e) {
            LOGGER.info("Error while adding vendor history : {}", e.getMessage());
        }

    }


    @Override
    public List<VendorAppResponseDTO> getAllFeaturedVendors(CoordinatesDTO coordinatesDTO) {
        List<VendorAppResponseDTO> responseDTOs = new ArrayList<>();
        int featuredVendorDistance = appConfig.getFeaturedVendorDistance();
        List<Vendor> vendors = vendorRepository.getFeaturedVendorList(coordinatesDTO, featuredVendorDistance);
        for (Vendor vendor : vendors) {
            responseDTOs.add(vendorMapper.toAppDto(vendor, false));
        }
        return responseDTOs;
    }


    /**
     * upload image of vendor
     *
     * @param image
     * @param vendor
     * @throws ValidationException
     * @throws FileOperationException
     */
    private void uploadImage(final MultipartFile image, final Vendor vendor) throws FileOperationException, ValidationException {
        LOGGER.info("Inside uploadImage for product :{}", vendor.getId());
        vendor.setStoreImageName(assetService.saveAsset(image, AssetConstant.VENDOR, 0, Constant.VENDOR_LIST_IMAGE_WIDTH, Constant.VENDOR_LIST_IMAGE_HEIGHT));
        vendor.setStoreImageOriginalName(image.getOriginalFilename());
        LOGGER.info("After uploadImage for product :{}", vendor.getId());
    }

    private void uploadDetailImage(final MultipartFile detailImage, final Vendor vendor) throws FileOperationException, ValidationException {
        LOGGER.info("Inside uploadImage for product :{}", vendor.getId());
        vendor.setStoreDetailImageName(assetService.saveAsset(detailImage, AssetConstant.VENDOR, 0, Constant.VENDOR_DETAIL_IMAGE_WIDTH, Constant.VENDOR_DETAIL_IMAGE_HEIGHT));
        vendor.setStoreDetailImageOriginalName(detailImage.getOriginalFilename());
        LOGGER.info("After uploadImage for product :{}", vendor.getId());
    }

    private void uploadFeaturedImage(final MultipartFile featuredImage, final Vendor vendor) throws FileOperationException, ValidationException {
        LOGGER.info("Inside uploadImage for product :{}", vendor.getId());
        vendor.setFeaturedImageName(assetService.saveAsset(featuredImage, AssetConstant.VENDOR, 0, Constant.VENDOR_FEATURE_IMAGE_WIDTH, Constant.VENDOR_FEATURE_IMAGE_HEIGHT));
        vendor.setFeaturedImageOriginalName(featuredImage.getOriginalFilename());
        LOGGER.info("After uploadImage for product :{}", vendor.getId());
    }


}
