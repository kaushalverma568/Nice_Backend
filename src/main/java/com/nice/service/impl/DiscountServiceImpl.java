package com.nice.service.impl;

import java.sql.Date;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.AssetConstant;
import com.nice.constant.Constant;
import com.nice.constant.DiscountStatusEnum;
import com.nice.constant.UserType;
import com.nice.dto.DiscountDTO;
import com.nice.dto.DiscountResponseDTO;
import com.nice.dto.ProductParamRequestDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.DiscountMapper;
import com.nice.model.Discount;
import com.nice.model.DiscountAppliedProductHistory;
import com.nice.model.Product;
import com.nice.model.ProductVariant;
import com.nice.model.UserLogin;
import com.nice.model.Vendor;
import com.nice.repository.DiscountAppliedProductHistoryRepository;
import com.nice.repository.DiscountRepository;
import com.nice.repository.ProductRepository;
import com.nice.repository.ProductVariantRepository;
import com.nice.service.AssetService;
import com.nice.service.CategoryService;
import com.nice.service.DiscountService;
import com.nice.service.ProductService;
import com.nice.service.ProductVariantService;
import com.nice.service.SchedulerDetailsService;
import com.nice.service.VendorService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
@Service("discountService")
@Transactional(rollbackFor = Throwable.class)
public class DiscountServiceImpl implements DiscountService {

	private static final String VALIDATION_EXCEPTION = "Validation exception. {}";

	private static final String NOT_FOUND_EXCEPTION = "Not found exception. {} ";

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	private static final Logger LOGGER = LoggerFactory.getLogger(DiscountServiceImpl.class);

	@Autowired
	private DiscountRepository discountRepository;

	@Autowired
	private DiscountAppliedProductHistoryRepository discountAppliedProductHistoryRepository;

	@Autowired
	private DiscountMapper discountMapper;

	@Autowired
	private ProductService productService;

	@Autowired
	private CategoryService categoryService;

	@Autowired
	private ProductRepository productRepository;

	@Autowired
	private ProductVariantService productVariantService;

	@Autowired
	private ProductVariantRepository productVariantRepository;

	@Autowired
	private SchedulerDetailsService schedulerDetailsService;

	@Autowired
	private AssetService assetService;

	@Autowired
	private VendorService vendorService;

	@Override
	public void addDiscount(final DiscountDTO discountDTO) throws ValidationException, NotFoundException {
		Discount discount = discountMapper.toEntity(discountDTO);
		final LocalDate todayLocalDate = LocalDateTime.now().toLocalDate();
		final LocalDate startDate = discount.getStartDate().toLocalDate();
		if (startDate.isAfter(discountDTO.getEndDate().toLocalDate())) {
			throw new ValidationException(messageByLocaleService.getMessage("end.date.invalid", null));
		} else
		/**
		 * if start date is current date then activate discount
		 */
		if (startDate.equals(todayLocalDate)) {
			discount.setStatus(DiscountStatusEnum.ACTIVE.getStatusValue());
			discount = discountRepository.save(discount);
			setDiscountForRelatedProduct(discountDTO, discount, true);
			/**
			 * if start date is greater then current date then upcoming discount
			 */
		} else if (startDate.isAfter(todayLocalDate)) {
			discount.setStatus(DiscountStatusEnum.UPCOMING.getStatusValue());
			discount = discountRepository.save(discount);
			setDiscountForRelatedProduct(discountDTO, discount, true);
		} else {
			LOGGER.error("Start date is invalid {}", startDate);
			throw new ValidationException(messageByLocaleService.getMessage("start.date.invalid", null));
		}
	}

	@Override
	public void updateDiscount(final DiscountDTO discountDTO) throws ValidationException, NotFoundException {
		/**
		 * Check related to updating discount
		 */
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();

		if (discountDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("discount.id.not.null", null));
		} else {
			final Discount existingDiscount = getDiscountDetails(discountDTO.getId());
			if (!UserType.VENDOR.name().equals(userLogin.getEntityType()) || !userLogin.getEntityId().equals(existingDiscount.getVendorId())) {
				throw new ValidationException(messageByLocaleService.getMessage("unauthorized", null));
			}

			if (!DiscountStatusEnum.UPCOMING.getStatusValue().equals(existingDiscount.getStatus())) {
				throw new ValidationException(messageByLocaleService.getMessage("discount.can.not.update", new Object[] { existingDiscount.getStatus() }));
			}
			final Discount discount = discountMapper.toEntity(discountDTO);
			final LocalDate todayLocalDate = LocalDateTime.now().toLocalDate();
			final LocalDate startDate = discount.getStartDate().toLocalDate();
			/**
			 * all the validation are same as add discount
			 */
			if (startDate.isAfter(discountDTO.getEndDate().toLocalDate())) {
				throw new ValidationException(messageByLocaleService.getMessage("end.date.invalid", null));
			} else if (startDate.equals(todayLocalDate)) {
				discount.setStatus(DiscountStatusEnum.ACTIVE.getStatusValue());
				setDiscountForRelatedProduct(discountDTO, discount, false);
			} else if (startDate.isAfter(todayLocalDate)) {
				discount.setStatus(DiscountStatusEnum.UPCOMING.getStatusValue());
				setDiscountForRelatedProduct(discountDTO, discount, false);
			} else {
				LOGGER.error("Start date is invalid {}", startDate);
				throw new ValidationException(messageByLocaleService.getMessage("start.date.invalid", null));
			}
		}
	}

	/**
	 * @param discountDTO
	 * @param discount
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	private void setDiscountForRelatedProduct(final DiscountDTO discountDTO, final Discount discount, final Boolean isCreation)
			throws NotFoundException, ValidationException {

		List<Long> productIds = discountDTO.getProductIds();
		List<Product> productList = productRepository.findAllById(productIds);
		/**
		 * if products do not have unique category then throw exception
		 */
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(productList)) {
			List<Long> categoryIds = productList.stream().map(Product::getCategoryId).distinct().collect(Collectors.toList());
			if (!categoryIds.isEmpty() && categoryIds.size() > 1) {
				throw new ValidationException(messageByLocaleService.getMessage("category.not.unique", null));
			}
		}
		/**
		 * in discount updation if we are removing some product for this discount then
		 * remove them from product history (all the other things are same as we written
		 * for category specific discount)
		 */
		if (!isCreation.booleanValue()) {
			/**
			 * remove discount history for product which is not in updated product list
			 */
			List<DiscountAppliedProductHistory> discountAppliedProductHistoryList = discountAppliedProductHistoryRepository
					.findAllByDiscountId(discount.getId());
			final List<Long> removeDiscountList = discountAppliedProductHistoryList.stream().map(DiscountAppliedProductHistory::getProductId)
					.collect(Collectors.toList());
			removeDiscountList.removeAll(discountDTO.getProductIds());
			for (Long productId : removeDiscountList) {
				discountAppliedProductHistoryRepository.deleteByProductIdAndDiscountId(productId, discount.getId());
			}
		}
		/**
		 * if product have an upcoming/active discount then throw exception (if old
		 * discount start date is between new start date & end date or end date is
		 * between new start date & end date)
		 */
		for (Long productId : discountDTO.getProductIds()) {
			Optional<DiscountAppliedProductHistory> existingHistory = discountAppliedProductHistoryRepository.isDiscountExist(productId, discount.getId(),
					discountDTO.getStartDate(), discountDTO.getEndDate());
			if (existingHistory.isPresent()) {
				throw new ValidationException(messageByLocaleService.getMessage("discount.exist", new Object[] { existingHistory.get().getProductId() }));
			}
			Optional<DiscountAppliedProductHistory> existingDiscountAppliedProductHistory = discountAppliedProductHistoryRepository
					.findByProductIdAndDiscountId(productId, discount.getId());
			if (!existingDiscountAppliedProductHistory.isPresent()) {
				DiscountAppliedProductHistory discountAppliedProductHistory = new DiscountAppliedProductHistory();
				discountAppliedProductHistory.setProductId(productId);
				discountAppliedProductHistory.setDiscountId(discount.getId());
				discountAppliedProductHistory.setStartDate(discount.getStartDate());
				discountAppliedProductHistory.setEndDate(discount.getEndDate());
				discountAppliedProductHistory.setStatus(discount.getStatus());
				discountAppliedProductHistoryRepository.save(discountAppliedProductHistory);
			} else {
				existingDiscountAppliedProductHistory.get().setStartDate(discount.getStartDate());
				existingDiscountAppliedProductHistory.get().setEndDate(discount.getEndDate());
				existingDiscountAppliedProductHistory.get().setStatus(discount.getStatus());
				discountAppliedProductHistoryRepository.save(existingDiscountAppliedProductHistory.get());
			}
			if (DiscountStatusEnum.ACTIVE.getStatusValue().equals(discount.getStatus())) {
				final Product product = productService.getProductDetail(productId);
				final List<ProductVariant> productVariantList = productVariantService.getProductVariantByProduct(product, null);
				for (ProductVariant productVariant : productVariantList) {
					final Double discounteRate = discountDTO.getDiscountRate();
					productVariant.setDiscountedRate(productVariant.getRate() - ((productVariant.getRate() * discounteRate) / 100));
				}
				productVariantRepository.saveAll(productVariantList);
				product.setDiscountId(discount.getId());
				productRepository.save(product);
			}
		}
		discount.setTotalProducts(discountDTO.getProductIds().size());
		discountRepository.save(discount);
	}

	/**
	 * @param discountId
	 * @param status
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	private void removeDiscountFromProducts(final Long discountId, final String status) throws NotFoundException, ValidationException {
		LOGGER.info("inside remove disount from product discountId:{}", discountId);
		/**
		 * if we are expiring discount then remove discount from product
		 */
		final ProductParamRequestDTO paramRequestDTO = new ProductParamRequestDTO();
		paramRequestDTO.setDiscountId(discountId);
		final List<Product> productList = productService.getProductListBasedOnParamsWithoutPagination(paramRequestDTO);
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(productList)) {
			for (Product product : productList) {
				final List<ProductVariant> productVariantList = productVariantService.getProductVariantByProduct(product, null);
				for (ProductVariant productVariant : productVariantList) {
					productVariant.setDiscountedRate(null);
				}
				productVariantRepository.saveAll(productVariantList);
				product.setDiscountId(null);
				productRepository.save(product);
			}
		} else {
			/**
			 * there is the case that product does not exist with discount if we directly
			 * cancel from upcoming status
			 */
			LOGGER.info("product not exist with this discount");
		}
		/**
		 * update status in discount history
		 */
		List<DiscountAppliedProductHistory> discountAppliedProductHistoryList = discountAppliedProductHistoryRepository.findAllByDiscountId(discountId);
		for (DiscountAppliedProductHistory discountAppliedProductHistory : discountAppliedProductHistoryList) {
			discountAppliedProductHistory.setStatus(status);
			discountAppliedProductHistory.setEndDate(new Date(System.currentTimeMillis()));
		}
		discountAppliedProductHistoryRepository.saveAll(discountAppliedProductHistoryList);

	}

	@Override
	public DiscountResponseDTO getDiscount(final Long discountId) throws NotFoundException, ValidationException {
		/**
		 * Check related to updating discount
		 */
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		Discount existingDiscount = getDiscountDetails(discountId);
		if (userLogin.getEntityType() != null
				&& (!UserType.VENDOR.name().equals(userLogin.getEntityType()) || !userLogin.getEntityId().equals(existingDiscount.getVendorId()))) {
			throw new ValidationException(messageByLocaleService.getMessage("unauthorized", null));
		}

		return convertEntityToResponseDto((getDiscountDetails(discountId)));
	}

	/**
	 * @param discount
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	private DiscountResponseDTO convertEntityToResponseDto(final Discount discount) throws NotFoundException {
		final Locale locale = LocaleContextHolder.getLocale();
		final DiscountResponseDTO discountResponseDTO = new DiscountResponseDTO();
		Vendor vendor = vendorService.getVendorDetail(discount.getVendorId());
		BeanUtils.copyProperties(discount, discountResponseDTO);
		discountResponseDTO.setCategoryName(categoryService.getCategory(discount.getCategoryId()).getName());
		discountResponseDTO.setDiscountDate(discount.getCreatedAt());
		discountResponseDTO.setStoreNameArabic(vendor.getStoreNameArabic());
		discountResponseDTO.setStoreNameEnglish(vendor.getStoreNameEnglish());
		if (locale.getLanguage().equals("en")) {
			discountResponseDTO.setStoreName(vendor.getStoreNameEnglish());
		} else {
			discountResponseDTO.setStoreName(vendor.getStoreNameArabic());
		}
		discountResponseDTO.setStorePhoneNumber(vendor.getStorePhoneNumber());
		return discountResponseDTO;
	}

	@Override
	public Discount getDiscountDetails(final Long discountId) throws NotFoundException {
		return discountRepository.findById(discountId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("discount.not.found", new Object[] { discountId })));
	}

	@Override
	public void changeStatus(final Long discountId, final String status) throws ValidationException, NotFoundException {
		LOGGER.info("Inside change status of discount method discountId: {} status {} ", discountId, status);

		/**
		 * Check related to updating discount
		 */
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();

		final Discount existingDiscount = getDiscountDetails(discountId);
		LOGGER.info("Existing discount details {} ", existingDiscount);

		/**
		 * check related to discount
		 */
		if (!UserType.VENDOR.name().equals(userLogin.getEntityType()) || !userLogin.getEntityId().equals(existingDiscount.getVendorId())) {
			throw new ValidationException(messageByLocaleService.getMessage("unauthorized", null));
		}

		/**
		 * check new discount status is valid or not
		 */
		if (DiscountStatusEnum.getByValue(status) == null) {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.status.type", null));
		}
		final DiscountStatusEnum discountNewStatus = DiscountStatusEnum.valueOf(DiscountStatusEnum.getByValue(status).name());
		final DiscountStatusEnum discountOldStatus = DiscountStatusEnum.valueOf(DiscountStatusEnum.getByValue(existingDiscount.getStatus()).name());
		if (discountOldStatus.nextStatus() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.status.message", new Object[] { discountNewStatus.getStatusValue() }));
		}
		final List<DiscountStatusEnum> discountStatusList = Arrays.asList(discountOldStatus.nextStatus());
		/**
		 * Throws exception if next status requested is invalid
		 */
		if (!discountStatusList.contains(discountNewStatus)) {
			throw new ValidationException(messageByLocaleService.getMessage("invalid.status.message", new Object[] { discountNewStatus.getStatusValue() }));
		}
		/**
		 * activating discount from schedular then set value in variants
		 */
		if (discountNewStatus.equals(DiscountStatusEnum.ACTIVE)) {
			setDiscountInProducts(existingDiscount);
		} else if (discountNewStatus.equals(DiscountStatusEnum.CANCELLED) || discountNewStatus.equals(DiscountStatusEnum.EXPIRED)) {
			removeDiscountFromProducts(discountId, status);
			existingDiscount.setEndDate(new Date(System.currentTimeMillis()));
		}
		existingDiscount.setStatus(status);
		discountRepository.save(existingDiscount);
	}

	/**
	 * @param existingDiscount
	 * @throws NotFoundException
	 */
	private void setDiscountInProducts(final Discount existingDiscount) throws NotFoundException {
		/**
		 * get product ids from history table for which we want to apply discount
		 */
		List<DiscountAppliedProductHistory> discountAppliedProductHistoryList = discountAppliedProductHistoryRepository
				.findAllByDiscountId(existingDiscount.getId());
		for (Long productId : discountAppliedProductHistoryList.stream().map(DiscountAppliedProductHistory::getProductId).collect(Collectors.toList())) {
			final Product product = productService.getProductDetail(productId);
			final List<ProductVariant> productVariantList = productVariantService.getProductVariantByProduct(product, null);
			for (ProductVariant productVariant : productVariantList) {
				final Double discounteRate = existingDiscount.getDiscountRate();
				productVariant.setDiscountedRate(productVariant.getRate() - ((productVariant.getRate() * discounteRate) / 100));
			}
			productVariantRepository.saveAll(productVariantList);
			product.setDiscountId(existingDiscount.getId());
			productRepository.save(product);
		}
		/**
		 * update status in discount history
		 */
		for (DiscountAppliedProductHistory discountAppliedProductHistory : discountAppliedProductHistoryList) {
			discountAppliedProductHistory.setStatus(DiscountStatusEnum.ACTIVE.getStatusValue());
		}
		discountAppliedProductHistoryRepository.saveAll(discountAppliedProductHistoryList);
	}

	@Override
	public Page<Discount> getDiscountListBasedOnParams(final Integer pageNumber, final Integer pageSize, final String status, Long vendorId) {

		/**
		 * Check related to updating discount
		 */
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();

		if (UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			vendorId = userLogin.getEntityId();
		}

		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("id"));
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(status) && vendorId != null) {
			return discountRepository.findAllByStatusAndVendorId(status, vendorId, pageable);
		} else if (status != null) {
			return discountRepository.findAllByStatus(status, pageable);
		} else if (vendorId != null) {
			return discountRepository.findAllByVendorId(vendorId, pageable);
		}
		return discountRepository.findAll(pageable);
	}

	@Override
	public List<DiscountResponseDTO> getDiscountListBasedOnParams(final List<Discount> discounts) throws NotFoundException, ValidationException {
		List<DiscountResponseDTO> discountResponseDTOs = new ArrayList<>();
		for (Discount discount : discounts) {
			discountResponseDTOs.add(convertEntityToResponseDto(discount));
		}
		return discountResponseDTOs;
	}

	@Override
	public void activateExpireDiscount(final LocalDate runDate) {
		final List<Discount> discountList = discountRepository.findAll();
		discountList.stream().forEach(discount -> {
			final LocalDate endDate = discount.getEndDate().toLocalDate();
			final LocalDate startDate = discount.getStartDate().toLocalDate();
			/**
			 * expire discount if it is active and run date is after end date
			 */
			if (runDate.isAfter(endDate) && discount.getStatus().equals(DiscountStatusEnum.ACTIVE.getStatusValue())) {
				try {
					changeStatus(discount.getId(), DiscountStatusEnum.EXPIRED.getStatusValue());
				} catch (final NotFoundException e) {
					LOGGER.error(NOT_FOUND_EXCEPTION, e);
				} catch (final ValidationException e) {
					LOGGER.error(VALIDATION_EXCEPTION, e);
				}
			}

			/**
			 * active discount if it is upcoming and run date is today
			 */
			if (runDate.isEqual(startDate) && discount.getStatus().equals(DiscountStatusEnum.UPCOMING.getStatusValue())) {
				try {
					changeStatus(discount.getId(), DiscountStatusEnum.ACTIVE.getStatusValue());
				} catch (final NotFoundException e) {
					LOGGER.error(NOT_FOUND_EXCEPTION, e);
				} catch (final ValidationException e) {
					LOGGER.error(VALIDATION_EXCEPTION, e);
				}
			}
			/**
			 * cancel discount if run date is after end date and still in upcoming state
			 * (this could be happend if scheduler will not run for some day )
			 */
			if (runDate.isAfter(endDate) && discount.getStatus().equals(DiscountStatusEnum.UPCOMING.getStatusValue())) {
				try {
					changeStatus(discount.getId(), DiscountStatusEnum.CANCELLED.getStatusValue());
				} catch (final NotFoundException e) {
					LOGGER.error(NOT_FOUND_EXCEPTION, e.getMessage());
				} catch (final ValidationException e) {
					LOGGER.error(VALIDATION_EXCEPTION, e.getMessage());
				}
			}

		});
		updateRunDate(runDate);
	}

	private void updateRunDate(final LocalDate runDate) {
		try {
			schedulerDetailsService.updateSchedulerDate(Constant.ACTIVATE_EXPIRE_DISCOUNT, CommonUtility.convertLocalDateToUtilDate(runDate));
		} catch (NotFoundException e) {
			LOGGER.error("Error while updating date of scheduler {} ", e.getMessage());
		}
		LOGGER.info("CurrencyRateScheduler -- End: The time is now {}", new Date(System.currentTimeMillis()));
	}

	@Override
	public Map<String, String> getProductListOfThatDiscount(final Long discountId) throws NotFoundException, ValidationException {
		Map<String, String> productMap = new HashMap<>();
		/**
		 * Check related to updating discount
		 */
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();

		Discount discount = getDiscountDetails(discountId);

		/**
		 * check related to discount
		 */
		if (UserType.VENDOR.name().equals(userLogin.getEntityType()) && !userLogin.getEntityId().equals(discount.getVendorId())) {
			throw new ValidationException(messageByLocaleService.getMessage("unauthorized", null));
		}

		List<DiscountAppliedProductHistory> discountAppliedProductHistoryList = discountAppliedProductHistoryRepository.findAllByDiscountId(discountId);
		for (Long productId : discountAppliedProductHistoryList.stream().map(DiscountAppliedProductHistory::getProductId).collect(Collectors.toList())) {
			final Product product = productService.getProductDetail(productId);
			if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
				productMap.put(assetService.getGeneratedUrl(product.getImage(), AssetConstant.PRODUCT_DIR), product.getNameEnglish());

			} else {
				productMap.put(assetService.getGeneratedUrl(product.getImage(), AssetConstant.PRODUCT_DIR), product.getNameArabic());

			}
		}
		return productMap;
	}
}
