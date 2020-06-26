package com.nice.service.impl;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
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
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.nice.constant.AssetConstant;
import com.nice.dto.CategoryDTO;
import com.nice.dto.CategoryImport;
import com.nice.dto.CategoryResponseDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.CategoryMapper;
import com.nice.model.Category;
import com.nice.model.SubCategory;
import com.nice.repository.CategoryRepository;
import com.nice.service.AssetService;
import com.nice.service.CategoryService;
import com.nice.service.FileStorageService;
import com.nice.service.SubCategoryService;
import com.nice.util.CSVProcessor;
import com.nice.util.CommonUtility;
import com.nice.util.ExportCSV;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
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
	private CategoryMapper categoryMapper;

	@Autowired
	private AssetService assetService;

	@Autowired
	private ExportCSV exportCSV;

	@Autowired
	private FileStorageService fileStorageService;

	@Override
	public void addCategory(final CategoryDTO categoryDTO, final MultipartFile image) throws ValidationException, NotFoundException {
		final Category category = categoryMapper.toEntity(categoryDTO);
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
			final Category category = categoryMapper.toEntity(resultCategoryDTO);
			if (image != null) {
				deleteOldImage(existingCategory);
				uploadImage(image, category);
			}
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
	public Page<Category> getCategoryList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final String searchKeyword)
			throws NotFoundException {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("name"));
		if (activeRecords != null) {
			if (searchKeyword != null) {
				return categoryRepository.findAllByActiveAndNameContainingIgnoreCase(activeRecords, searchKeyword, pageable);
			} else {
				return categoryRepository.findAllByActive(activeRecords, pageable);
			}
		} else {
			if (searchKeyword != null) {
				return categoryRepository.findAllByNameContainingIgnoreCase(searchKeyword, pageable);
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
			} else {
				LOGGER.info("Activating  Category");
			}
			existingCategory.setActive(active);
			categoryRepository.save(existingCategory);
		}
	}

	@Override
	public Boolean isCategoryExists(final CategoryDTO categoryDTO) {
		if (categoryDTO.getId() != null) {
			/**
			 * At the time of update is category with same name exist or not except it's own
			 * id
			 */
			return categoryRepository.findByNameIgnoreCaseAndIdNot(categoryDTO.getName(), categoryDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is category with same name exist or not
			 */
			return categoryRepository.findByNameIgnoreCase(categoryDTO.getName()).isPresent();
		}
	}

	@Override
	public void deleteCategory(final Long categoryId) throws NotFoundException, ValidationException {
		LOGGER.info("Inside delete Category for id:{}", categoryId);
		final Category category = getCategoryDetail(categoryId);
		final List<SubCategory> subCategories = subCategoryService.getSubCategoryListByCategory(category);
		for (final SubCategory subCategory : subCategories) {
			subCategoryService.deleteSubCategory(subCategory.getId());
		}
		deleteOldImage(category);
		categoryRepository.deleteById(categoryId);
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
			fileStorageService.deleteFile(category.getImage(), AssetConstant.CATEGORY);
		}
	}

	@Override
	public void exportCategoryList(final HttpServletResponse httpServletResponse) throws FileOperationException {
		final List<CategoryResponseDTO> categoryList = categoryMapper.toDtos(categoryRepository.findAll());
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
						categoryImports.stream().filter(x -> CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(x.getName())).collect(Collectors.toList()));
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
		final List<CategoryImport> allResult = new ArrayList<>();
		for (CategoryImport categoryImport : categoryImports) {
			try {
				if (categoryRepository.findByNameIgnoreCase(categoryImport.getName()).isPresent()) {
					throw new ValidationException(messageByLocaleService.getMessage("category.name.not.unique", null));
				} else {
					final CategoryDTO categoryDTO = new CategoryDTO();
					categoryDTO.setName(categoryImport.getName());
					categoryDTO.setActive(true);
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
}
