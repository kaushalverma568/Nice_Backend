package com.nice.service.impl;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
import com.nice.constant.DiscountStatusEnum;
import com.nice.constant.UserType;
import com.nice.dto.CategoryDTO;
import com.nice.dto.CategoryImport;
import com.nice.dto.CategoryResponseDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.CategoryMapper;
import com.nice.model.Category;
import com.nice.model.Discount;
import com.nice.model.SubCategory;
import com.nice.model.UserLogin;
import com.nice.model.Vendor;
import com.nice.repository.CategoryRepository;
import com.nice.repository.DiscountRepository;
import com.nice.service.AssetService;
import com.nice.service.CategoryService;
import com.nice.service.DiscountService;
import com.nice.service.FileStorageService;
import com.nice.service.SubCategoryService;
import com.nice.service.VendorService;
import com.nice.util.CSVProcessor;
import com.nice.util.CommonUtility;
import com.nice.util.ExportCSV;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("categoryService")
public class CategoryServiceImpl implements CategoryService {

	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(CategoryServiceImpl.class);

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private CategoryRepository categoryRepository;

	@Autowired
	private SubCategoryService subCategoryService;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private CategoryMapper categoryMapper;

	@Autowired
	private AssetService assetService;

	@Autowired
	private ExportCSV exportCSV;

	@Autowired
	private DiscountRepository discountRepository;

	@Autowired
	private DiscountService discountService;

	@Autowired
	private FileStorageService fileStorageService;

	@Override
	public void addCategory(final CategoryDTO categoryDTO, final MultipartFile image) throws ValidationException, NotFoundException {
		final Category category = categoryMapper.toEntity(categoryDTO);
		category.setVendor(vendorService.getVendorDetail(categoryDTO.getVendorId()));
		if (image != null) {
			uploadImage(image, category);
		}
		categoryRepository.save(category);
	}

	@Override
	public void updateCategory(final CategoryDTO resultCategoryDTO, final MultipartFile image) throws NotFoundException, ValidationException {
		if (resultCategoryDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("category.id.not.null", null));
		} else {
			final Category existingCategory = getCategoryDetail(resultCategoryDTO.getId());
			if (!existingCategory.getVendor().getId().equals(resultCategoryDTO.getVendorId())) {
				throw new ValidationException(messageByLocaleService.getMessage("vendor.id.not.unique", null));
			}
			final Category category = categoryMapper.toEntity(resultCategoryDTO);
			if (image != null && CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(image.getOriginalFilename())) {
				deleteOldImage(existingCategory);
				uploadImage(image, category);
			} else {
				category.setImage(existingCategory.getImage());
				category.setImageOriginalName(existingCategory.getImageOriginalName());
			}
			category.setVendor(existingCategory.getVendor());
			categoryRepository.save(category);
		}
	}

	@Override
	public CategoryResponseDTO getCategory(final Long categoryId) throws NotFoundException {
		final Category existingCategory = getCategoryDetail(categoryId);
		return categoryMapper.toDto(existingCategory);
	}

	@Override
	public Category getCategoryDetail(final Long categoryId) throws NotFoundException {
		return categoryRepository.findById(categoryId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("category.not.found", new Object[] { categoryId })));
	}

	@Override
	public Page<Category> getCategoryList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final String searchKeyword,
			final Long vendorId) throws NotFoundException {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("name"));
		if (vendorId != null) {
			Vendor vendor = vendorService.getVendorDetail(vendorId);
			if (activeRecords != null) {
				if (searchKeyword != null) {
					return categoryRepository.findAllByActiveAndNameEnglishContainingIgnoreCaseAndVendorOrActiveAndNameArabicContainingIgnoreCaseAndVendor(
							activeRecords, searchKeyword, vendor, activeRecords, searchKeyword, vendor, pageable);
				} else {
					return categoryRepository.findAllByActiveAndVendor(activeRecords, vendor, pageable);
				}
			} else {
				if (searchKeyword != null) {
					return categoryRepository.findAllByNameEnglishContainingIgnoreCaseAndVendorOrNameArabicContainingIgnoreCaseAndVendor(searchKeyword, vendor,
							searchKeyword, vendor, pageable);
				} else {
					return categoryRepository.findAllByVendor(vendor, pageable);
				}
			}
		} else {
			return findAllByActiveAndSearchKeyword(activeRecords, searchKeyword, pageable);
		}
	}

	/**
	 * @param activeRecords
	 * @param searchKeyword
	 * @param pageable
	 * @return
	 */
	private Page<Category> findAllByActiveAndSearchKeyword(final Boolean activeRecords, final String searchKeyword, final Pageable pageable) {
		if (activeRecords != null) {
			if (searchKeyword != null) {
				return categoryRepository.findAllByActiveAndNameEnglishContainingIgnoreCaseOrActiveAndNameArabicContainingIgnoreCase(activeRecords,
						searchKeyword, activeRecords, searchKeyword, pageable);
			} else {
				return categoryRepository.findAllByActive(activeRecords, pageable);
			}

		} else {
			if (searchKeyword != null) {
				return categoryRepository.findAllByNameEnglishContainingIgnoreCaseOrNameArabicContainingIgnoreCase(searchKeyword, searchKeyword, pageable);
			} else {
				return categoryRepository.findAll(pageable);
			}
		}
	}

	@Override
	public void changeStatus(final Long categoryId, final Boolean active) throws NotFoundException, ValidationException {
		final Category existingCategory = getCategoryDetail(categoryId);
		LOGGER.info("Existing category details {} ", existingCategory);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingCategory.getActive().equals(active)) {
			throw new ValidationException(messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "category.active" : "category.deactive", null));
		} else {
			/**
			 * deActive All subCategories related to this category at the time of
			 * deActivating Category
			 */
			if (Boolean.FALSE.equals(active)) {
				LOGGER.info("DeActivating  Category {}", existingCategory);
				final List<SubCategory> subCategories = subCategoryService.getSubCategoryListByCategoryAndActive(existingCategory, Boolean.TRUE);
				for (final SubCategory subCategory : subCategories) {
					subCategoryService.changeStatus(subCategory.getId(), Boolean.FALSE);
				}

				/**
				 * deActive All discount related to this category at the time of deActivating
				 * Brand
				 */
				List<Discount> discountList = discountRepository.findAllByCategoryIdAndStatusIn(categoryId,
						Arrays.asList(DiscountStatusEnum.ACTIVE.getStatusValue(), DiscountStatusEnum.UPCOMING.getStatusValue()));
				if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(discountList)) {
					for (Discount discount : discountList) {
						discountService.changeStatus(discount.getId(), DiscountStatusEnum.CANCELLED.getStatusValue());
					}
				}
			} else {
				LOGGER.info("Activating  Category");
			}
			existingCategory.setActive(active);
			categoryRepository.save(existingCategory);
		}
	}

	@Override
	public Boolean isCategoryExists(final CategoryDTO categoryDTO) throws NotFoundException {
		Vendor vendor = vendorService.getVendorDetail(categoryDTO.getVendorId());
		if (categoryDTO.getId() != null) {
			/**
			 * At the time of update is category with same name exist or not except it's own
			 * id
			 */
			return categoryRepository.findByNameEnglishIgnoreCaseAndVendorAndIdNotOrNameArabicIgnoreCaseAndVendorAndIdNot(categoryDTO.getNameEnglish(), vendor,
					categoryDTO.getId(), categoryDTO.getNameArabic(), vendor, categoryDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is category with same name exist or not
			 */
			return categoryRepository.findByNameEnglishIgnoreCaseAndVendorOrNameArabicIgnoreCaseAndVendor(categoryDTO.getNameEnglish(), vendor,
					categoryDTO.getNameArabic(), vendor).isPresent();
		}
	}

	/**
	 * upload image of product
	 *
	 * @param image
	 * @param product
	 */
	private void uploadImage(final MultipartFile image, final Category category) {
		category.setImage(assetService.saveAsset(image, AssetConstant.CATEGORY, 0));
		category.setImageOriginalName(image.getOriginalFilename());
	}

	/**
	 * delete old image
	 *
	 * @param product
	 */
	private void deleteOldImage(final Category category) {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(category.getImage())) {
			assetService.deleteFile(category.getImage(), AssetConstant.CATEGORY);
		}
	}

	@Override
	public void exportCategoryList(final HttpServletResponse httpServletResponse) throws FileOperationException, ValidationException, NotFoundException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (!UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		Vendor vendor = vendorService.getVendorDetail(userLogin.getEntityId());
		final List<CategoryResponseDTO> categoryList = categoryMapper.toDtos(categoryRepository.findAllByVendor(vendor));
		final Object[] categoryHeaderField = new Object[] { "Category Name", "Active" };
		final Object[] categoryDataField = new Object[] { "name", "active" };
		try {
			exportCSV.writeCSVFile(categoryList, categoryDataField, categoryHeaderField, httpServletResponse);
		} catch (IOException e) {
			throw new FileOperationException(messageByLocaleService.getMessage("export.file.error", null));
		}
	}

	@Override
	public void uploadFile(final MultipartFile multipartFile, final HttpServletResponse httpServletResponse) throws FileOperationException {
		final String fileName = fileStorageService.storeFile(multipartFile, "category", AssetConstant.CATEGORY);
		Path filePath = fileStorageService.getOriginalFilePath(fileName, AssetConstant.CATEGORY);
		final File file = new File(filePath.toString());
		final CSVProcessor<CategoryImport> csvProcessor = new CSVProcessor<>();
		try {
			final List<CategoryImport> categoryImports = csvProcessor.convertCSVFileToListOfBean(file, CategoryImport.class);
			if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(categoryImports)) {
				final List<CategoryImport> insertListOfBean = insertListOfCategories(
						categoryImports.stream().filter(x -> CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(x.getNameEnglish())
								&& CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(x.getNameArabic())).collect(Collectors.toList()));
				Object[] categoryDetailsHeadersField = new Object[] { "Category Name", "Result" };
				Object[] categoryDetailsField = new Object[] { "name", "uploadMessage" };
				exportCSV.writeCSVFile(insertListOfBean, categoryDetailsField, categoryDetailsHeadersField, httpServletResponse);
			}
		} catch (SecurityException | IOException e) {
			throw new FileOperationException(messageByLocaleService.getMessage("import.file.error", null));
		}
	}

	/**
	 * @param categoryImports
	 * @param userId
	 * @return
	 */
	private List<CategoryImport> insertListOfCategories(final List<CategoryImport> categoryImports) {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		final List<CategoryImport> allResult = new ArrayList<>();
		for (CategoryImport categoryImport : categoryImports) {
			try {
				if (!UserType.VENDOR.name().equals(userLogin.getEntityType())) {
					throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
				}
				Vendor vendor = vendorService.getVendorDetail(userLogin.getEntityId());
				if (categoryRepository.findByNameEnglishIgnoreCaseAndVendorOrNameArabicIgnoreCaseAndVendor(categoryImport.getNameEnglish(), vendor,
						categoryImport.getNameArabic(), vendor).isPresent()) {
					throw new ValidationException(messageByLocaleService.getMessage("category.name.not.unique", null));
				} else {
					final CategoryDTO categoryDTO = new CategoryDTO();
					categoryDTO.setNameEnglish(categoryImport.getNameEnglish());
					categoryDTO.setNameArabic(categoryImport.getNameArabic());
					categoryDTO.setActive(true);
					categoryDTO.setVendorId(vendor.getId());
					addCategory(categoryDTO, null);
					categoryImport.setUploadMessage(messageByLocaleService.getMessage("upload.success", null));
				}
			} catch (Exception e) {
				categoryImport.setUploadMessage(messageByLocaleService.getMessage("upload.failure", new Object[] { e.getMessage() }));
			}
			allResult.add(categoryImport);
		}
		return allResult;
	}

	@Override
	public void deleteImage(final Long categoryId) throws NotFoundException {
		Category category = getCategoryDetail(categoryId);
		deleteOldImage(category);
		category.setImage(null);
		category.setImageOriginalName(null);
		categoryRepository.save(category);
	}

	@Override
	public List<Category> getCategoryListByVendor(final Long vendorId) throws NotFoundException {
		Vendor vendor = vendorService.getVendorDetail(vendorId);
		return categoryRepository.findAllByVendor(vendor);
	}

}
