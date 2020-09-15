package com.nice.service.impl;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
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
import com.nice.constant.UserType;
import com.nice.dto.SubCategoryDTO;
import com.nice.dto.SubCategoryImport;
import com.nice.dto.SubCategoryResponseDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.SubCategoryMapper;
import com.nice.model.Category;
import com.nice.model.SubCategory;
import com.nice.model.UserLogin;
import com.nice.model.Vendor;
import com.nice.repository.CategoryRepository;
import com.nice.repository.SubCategoryRepository;
import com.nice.service.AssetService;
import com.nice.service.CategoryService;
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
@Service("subCategoryService")
public class SubCategoryServiceImpl implements SubCategoryService {
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(SubCategoryServiceImpl.class);

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private SubCategoryRepository subCategoryRepository;

	@Autowired
	private CategoryRepository categoryRepository;

	@Autowired
	private CategoryService categoryService;

	@Autowired
	private SubCategoryMapper subCategoryMapper;

	@Autowired
	private ExportCSV exportCSV;

	@Autowired
	private AssetService assetService;

	@Autowired
	private FileStorageService fileStorageService;

	@Autowired
	private VendorService vendorService;

	@Override
	public void addSubCategory(final SubCategoryDTO resultSubCategoryDTO, final MultipartFile image)
			throws ValidationException, NotFoundException, FileOperationException {
		final SubCategory subCategory = subCategoryMapper.toEntity(resultSubCategoryDTO);
		if (image != null) {
			uploadImage(image, subCategory);
		}
		final Category category = categoryService.getCategoryDetail(resultSubCategoryDTO.getCategoryId());
		subCategory.setCategory(category);
		subCategoryRepository.save(subCategory);
	}

	@Override
	public void updateSubCategory(final SubCategoryDTO resultSubCategoryDTO, final MultipartFile image)
			throws NotFoundException, ValidationException, FileOperationException {
		if (resultSubCategoryDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("subcategory.id.not.null", null));
		} else {
			final SubCategory existingSubCategory = getSubCategoryDetail(resultSubCategoryDTO.getId());
			if (!existingSubCategory.getCategory().getId().equals(resultSubCategoryDTO.getCategoryId())) {
				throw new ValidationException(messageByLocaleService.getMessage("category.id.not.unique", null));
			} else {
				final SubCategory subCategory = subCategoryMapper.toEntity(resultSubCategoryDTO);
				if (image != null && CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(image.getOriginalFilename())) {
					deleteOldImage(existingSubCategory);
					uploadImage(image, subCategory);
				} else {
					subCategory.setImage(existingSubCategory.getImage());
					subCategory.setImageOriginalName(existingSubCategory.getImageOriginalName());
				}
				subCategory.setCategory(existingSubCategory.getCategory());
				subCategoryRepository.save(subCategory);
			}
		}
	}

	@Override
	public SubCategoryResponseDTO getSubCategory(final Long subCategoryId) throws NotFoundException {
		final SubCategory existingSubCategory = getSubCategoryDetail(subCategoryId);
		return subCategoryMapper.toDto(existingSubCategory);
	}

	@Override
	public SubCategory getSubCategoryDetail(final Long subCategoryId) throws NotFoundException {
		return subCategoryRepository.findById(subCategoryId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("subcategory.not.found", new Object[] { subCategoryId })));
	}

	@Override
	public Page<SubCategory> getSubCategoryList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final Long categoryId)
			throws NotFoundException {
		Pageable pageable;
		Locale locale = LocaleContextHolder.getLocale();
		if (locale.getLanguage().equals("en")) {
			pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("nameEnglish"));
		} else {
			pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("nameArabic"));
		}
		if (categoryId != null) {
			final Category category = categoryService.getCategoryDetail(categoryId);
			if (activeRecords != null) {
				return subCategoryRepository.findAllByActiveAndCategory(activeRecords, category, pageable);
			} else {
				return subCategoryRepository.findAllByCategory(category, pageable);
			}
		} else if (activeRecords != null) {
			return subCategoryRepository.findAllByActive(activeRecords, pageable);
		} else {
			return subCategoryRepository.findAll(pageable);
		}
	}

	@Override
	public List<SubCategory> getSubCategoryListByCategoryAndActive(final Category category, final Boolean active)
			throws NotFoundException, ValidationException {
		if (category.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("category.id.not.null", null));
		} else if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else {
			return subCategoryRepository.findByCategoryAndActive(category, active);
		}
	}

	@Override
	public List<SubCategory> getSubCategoryListByCategory(final Category category) throws NotFoundException, ValidationException {
		if (category.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("category.id.not.null", null));
		} else {
			return subCategoryRepository.findAllByCategory(category);
		}
	}

	@Override
	public void changeStatus(final Long subCategoryId, final Boolean active) throws NotFoundException, ValidationException {

		SubCategory existingSubCategory = subCategoryRepository.findById(subCategoryId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("subcategory.not.found", new Object[] { subCategoryId })));
		LOGGER.info("Existing SubCategory details {} ", existingSubCategory);

		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingSubCategory.getActive().equals(active)) {
			throw new ValidationException(messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "subcategory.active" : "subcategory.deactive", null));
			/**
			 * If category is not active then can not active this subCategory
			 */
		} else if (Boolean.TRUE.equals(active) && Boolean.FALSE.equals(existingSubCategory.getCategory().getActive())) {
			throw new ValidationException(messageByLocaleService.getMessage("category.activate.first", null));
		} else {
			if (Boolean.FALSE.equals(active)) {
				/**
				 * deactive dependent entities here like product
				 */
			}
			existingSubCategory.setActive(active);
			subCategoryRepository.save(existingSubCategory);
		}
	}

	@Override
	public Boolean isSubCategoryExistsEnglish(final SubCategoryDTO subCategoryDTO) throws NotFoundException {
		Category category = categoryService.getCategoryDetail(subCategoryDTO.getCategoryId());
		if (subCategoryDTO.getId() != null) {
			/**
			 * At the time of update is subCategory with same name exist or not
			 */
			return subCategoryRepository.findByNameEnglishIgnoreCaseAndCategoryAndIdNot(subCategoryDTO.getNameEnglish(), category, subCategoryDTO.getId())
					.isPresent();
		} else {
			/**
			 * At the time of create is subCategory with same name exist or not
			 */
			return subCategoryRepository.findByNameEnglishIgnoreCaseAndCategory(subCategoryDTO.getNameEnglish(), category).isPresent();
		}
	}

	@Override
	public Boolean isSubCategoryExistsArabic(final SubCategoryDTO subCategoryDTO) throws NotFoundException {
		Category category = categoryService.getCategoryDetail(subCategoryDTO.getCategoryId());
		if (subCategoryDTO.getId() != null) {
			/**
			 * At the time of update is subCategory with same name exist or not
			 */
			return subCategoryRepository.findByNameArabicIgnoreCaseAndCategoryAndIdNot(subCategoryDTO.getNameArabic(), category, subCategoryDTO.getId())
					.isPresent();
		} else {
			/**
			 * At the time of create is subCategory with same name exist or not
			 */
			return subCategoryRepository.findByNameArabicIgnoreCaseAndCategory(subCategoryDTO.getNameArabic(), category).isPresent();
		}
	}

	/**
	 * upload image of sub category
	 *
	 * @param image
	 * @param product
	 * @throws ValidationException
	 * @throws FileOperationException
	 */
	private void uploadImage(final MultipartFile image, final SubCategory subCategory) throws FileOperationException, ValidationException {
		subCategory
				.setImage(assetService.saveAsset(image, AssetConstant.SUB_CATEGORY, 0, Constant.SUB_CATEGORY_IMAGE_WIDTH, Constant.SUB_CATEGORY_IMAGE_HEIGHT));
		subCategory.setImageOriginalName(image.getOriginalFilename());
	}

	/**
	 * delete old image
	 *
	 * @param product
	 */
	private void deleteOldImage(final SubCategory subCategory) {
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(subCategory.getImage())) {
			assetService.deleteFile(subCategory.getImage(), AssetConstant.SUB_CATEGORY);
		}
	}

	@Override
	public void exportSubCategoryList(final HttpServletResponse httpServletResponse) throws FileOperationException, ValidationException, NotFoundException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (!UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		Vendor vendor = vendorService.getVendorDetail(userLogin.getEntityId());
		final List<SubCategoryResponseDTO> subCategoryList = subCategoryMapper.toDtos(subCategoryRepository.getAllByVendor(vendor));
		final Object[] subCategoryHeaderField = new Object[] { "Subcategory Name", "Category Name", "Active" };
		final Object[] subCategoryDataField = new Object[] { "name", "categoryName", "active" };
		try {
			exportCSV.writeCSVFile(subCategoryList, subCategoryDataField, subCategoryHeaderField, httpServletResponse);
		} catch (IOException e) {
			throw new FileOperationException(messageByLocaleService.getMessage("export.file.error", null));
		}
	}

	@Override
	public void uploadFile(final MultipartFile multipartFile, final HttpServletResponse httpServletResponse) throws FileOperationException {
		final String fileName = fileStorageService.storeFile(multipartFile, "sub_category", AssetConstant.SUB_CATEGORY);
		Path filePath = fileStorageService.getOriginalFilePath(fileName, AssetConstant.SUB_CATEGORY);
		final File file = new File(filePath.toString());
		final CSVProcessor<SubCategoryImport> csvProcessor = new CSVProcessor<>();
		try {
			final List<SubCategoryImport> subCategoryImports = csvProcessor.convertCSVFileToListOfBean(file, SubCategoryImport.class);
			if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(subCategoryImports)) {
				final List<SubCategoryImport> insertListOfBean = insertListOfSubCategories(subCategoryImports.stream()
						.filter(x -> CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(x.getNameEnglish())
								&& CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(x.getCategoryNameEnglish())
								&& CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(x.getNameArabic())
								&& CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(x.getCategoryNameArabic()))
						.collect(Collectors.toList()));
				Object[] subCategoryDetailsHeadersField = new Object[] { "SubCategory Name English", "SubCategory Name Arabic", "Category Name English",
						"Category Name Arabic", "Result" };
				Object[] subCategoryDetailsField = new Object[] { "nameEnglish", "nameArabic", "categoryNameEnglish", "categoryNameArabic", "uploadMessage" };
				exportCSV.writeCSVFile(insertListOfBean, subCategoryDetailsField, subCategoryDetailsHeadersField, httpServletResponse);
			}
		} catch (SecurityException | IOException e) {
			throw new FileOperationException(messageByLocaleService.getMessage("import.file.error", null));
		}
	}

	/**
	 * @param subCategoryImports
	 * @param userId
	 * @return
	 */
	private List<SubCategoryImport> insertListOfSubCategories(final List<SubCategoryImport> subCategoryImports) {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();

		final List<SubCategoryImport> allResult = new ArrayList<>();
		for (SubCategoryImport subCategoryImport : subCategoryImports) {
			try {
				if (!UserType.VENDOR.name().equals(userLogin.getEntityType())) {
					throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
				}
				Vendor vendor = vendorService.getVendorDetail(userLogin.getEntityId());
				Optional<Category> category = categoryRepository.findByNameEnglishIgnoreCaseAndNameArabicIgnoreCaseAndVendor(
						subCategoryImport.getCategoryNameEnglish(), subCategoryImport.getCategoryNameArabic(), vendor);
				if (!category.isPresent()) {
					throw new ValidationException(messageByLocaleService.getMessage("category.not.present",
							new Object[] { subCategoryImport.getCategoryNameEnglish().concat(" , ").concat(subCategoryImport.getCategoryNameArabic()) }));
				} else if (subCategoryRepository.findByNameEnglishIgnoreCaseAndCategory(subCategoryImport.getNameEnglish(), category.get()).isPresent()) {
					throw new ValidationException(messageByLocaleService.getMessage("subcategory.name.english.not.unique", null));
				} else if (subCategoryRepository.findByNameArabicIgnoreCaseAndCategory(subCategoryImport.getNameArabic(), category.get()).isPresent()) {
					throw new ValidationException(messageByLocaleService.getMessage("subcategory.name.arabic.not.unique", null));
				} else {
					final SubCategoryDTO subCategoryDTO = new SubCategoryDTO();
					subCategoryDTO.setNameEnglish(subCategoryImport.getNameEnglish());
					subCategoryDTO.setNameArabic(subCategoryImport.getNameArabic());
					subCategoryDTO.setCategoryId(category.get().getId());
					subCategoryDTO.setActive(true);
					addSubCategory(subCategoryDTO, null);
					subCategoryImport.setUploadMessage(messageByLocaleService.getMessage("upload.success", null));
				}
			} catch (Exception e) {
				subCategoryImport.setUploadMessage(messageByLocaleService.getMessage("upload.failure", new Object[] { e.getMessage() }));
			}
			allResult.add(subCategoryImport);
		}
		return allResult;
	}

	@Override
	public List<SubCategory> getSubCategoryList(final Boolean activeRecords, final String searchKeyword) {
		if (activeRecords != null) {
			if (searchKeyword != null) {
				return subCategoryRepository.findAllByActiveAndNameEnglishContainingIgnoreCaseOrActiveAndNameArabicContainingIgnoreCase(activeRecords,
						searchKeyword, activeRecords, searchKeyword);
			} else {
				return subCategoryRepository.findAllByActive(activeRecords);
			}
		} else {
			if (searchKeyword != null) {
				return subCategoryRepository.findAllByNameEnglishContainingIgnoreCaseOrNameArabicContainingIgnoreCase(searchKeyword, searchKeyword);
			} else {
				return subCategoryRepository.findAll();
			}
		}
	}

	@Override
	public void deleteImage(final Long subCategoryId) throws NotFoundException {
		SubCategory subCategory = getSubCategoryDetail(subCategoryId);
		deleteOldImage(subCategory);
		subCategory.setImage(null);
		subCategory.setImageOriginalName(null);
		subCategoryRepository.save(subCategory);
	}
}
