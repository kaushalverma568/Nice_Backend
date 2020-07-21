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
import com.nice.dto.BrandDTO;
import com.nice.dto.BrandImport;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.BrandMapper;
import com.nice.model.Brand;
import com.nice.repository.BrandRepository;
import com.nice.service.BrandService;
import com.nice.service.FileStorageService;
import com.nice.util.CSVProcessor;
import com.nice.util.CommonUtility;
import com.nice.util.ExportCSV;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 20-Jul-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("brandService")
public class BrandServiceImpl implements BrandService {

	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(BrandServiceImpl.class);

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private BrandRepository brandRepository;

	@Autowired
	private BrandMapper brandMapper;

	@Autowired
	private ExportCSV exportCSV;

	@Autowired
	private FileStorageService fileStorageService;

	@Override
	public void addBrand(final BrandDTO brandDTO) throws NotFoundException {
		brandRepository.save(brandMapper.toEntity(brandDTO));
	}

	@Override
	public void updateBrand(final BrandDTO resultBrandDTO) throws NotFoundException, ValidationException {
		if (resultBrandDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("brand.id.not.null", null));
		} else {
			getBrandDetail(resultBrandDTO.getId());
			brandRepository.save(brandMapper.toEntity(resultBrandDTO));
		}
	}

	@Override
	public BrandDTO getBrand(final Long brandId) throws NotFoundException {
		final Brand existingBrand = getBrandDetail(brandId);
		return brandMapper.toDto(existingBrand);
	}

	@Override
	public Brand getBrandDetail(final Long brandId) throws NotFoundException {
		return brandRepository.findById(brandId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("brand.not.found", new Object[] { brandId })));
	}

	@Override
	public Page<Brand> getBrandList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final String searchKeyword)
			throws NotFoundException {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("name"));
		if (activeRecords != null) {
			if (searchKeyword != null) {
				return brandRepository.findAllByActiveAndNameContainingIgnoreCase(activeRecords, searchKeyword, pageable);
			} else {
				return brandRepository.findAllByActive(activeRecords, pageable);
			}
		} else {
			if (searchKeyword != null) {
				return brandRepository.findAllByNameContainingIgnoreCase(searchKeyword, pageable);
			} else {
				return brandRepository.findAll(pageable);
			}
		}
	}

	@Override
	public void changeStatus(final Long brandId, final Boolean active) throws NotFoundException, ValidationException {
		final Brand existingBrand = getBrandDetail(brandId);
		LOGGER.info("Existing brand details {} ", existingBrand);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingBrand.getActive().equals(active)) {
			throw new ValidationException(messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "brand.active" : "brand.deactive", null));
		} else {
			/**
			 * deActive related entities at the time of deActivating Brand
			 */
			if (Boolean.FALSE.equals(active)) {
				LOGGER.info("DeActivating  Brand {}", existingBrand);
			}
			existingBrand.setActive(active);
			brandRepository.save(existingBrand);
		}
	}

	@Override
	public Boolean isBrandExists(final BrandDTO brandDTO) {
		if (brandDTO.getId() != null) {
			/**
			 * At the time of update is brand with same name exist or not except it's own id
			 */
			return brandRepository.findByNameIgnoreCaseAndIdNot(brandDTO.getName(), brandDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is brand with same name exist or not
			 */
			return brandRepository.findByNameIgnoreCase(brandDTO.getName()).isPresent();
		}
	}

	@Override
	public void exportBrandList(final HttpServletResponse httpServletResponse) throws FileOperationException {
		final List<BrandDTO> brandList = brandMapper.toDtos(brandRepository.findAll());
		final Object[] brandHeaderField = new Object[] { "Brand Name", "Active" };
		final Object[] brandDataField = new Object[] { "name", "active" };
		try {
			exportCSV.writeCSVFile(brandList, brandDataField, brandHeaderField, httpServletResponse);
		} catch (IOException e) {
			throw new FileOperationException(messageByLocaleService.getMessage("export.file.error", null));
		}
	}

	@Override
	public void uploadFile(final MultipartFile multipartFile, final HttpServletResponse httpServletResponse) throws FileOperationException {
		final String fileName = fileStorageService.storeFile(multipartFile, "brand", AssetConstant.BRAND);
		Path filePath = fileStorageService.getOriginalFilePath(fileName, AssetConstant.BRAND);
		final File file = new File(filePath.toString());
		final CSVProcessor<BrandImport> csvProcessor = new CSVProcessor<>();
		try {
			final List<BrandImport> brandImports = csvProcessor.convertCSVFileToListOfBean(file, BrandImport.class);
			if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(brandImports)) {
				final List<BrandImport> insertListOfBean = insertListOfBrands(
						brandImports.stream().filter(x -> CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(x.getName())).collect(Collectors.toList()));
				Object[] brandDetailsHeadersField = new Object[] { "Brand Name", "Result" };
				Object[] brandDetailsField = new Object[] { "name", "uploadMessage" };
				exportCSV.writeCSVFile(insertListOfBean, brandDetailsField, brandDetailsHeadersField, httpServletResponse);
			}
		} catch (SecurityException | IOException e) {
			throw new FileOperationException(messageByLocaleService.getMessage("import.file.error", null));
		}
	}

	/**
	 * @param  brandImports
	 * @param  userId
	 * @return
	 */
	private List<BrandImport> insertListOfBrands(final List<BrandImport> brandImports) {
		final List<BrandImport> allResult = new ArrayList<>();
		for (BrandImport brandImport : brandImports) {
			try {
				if (brandRepository.findByNameIgnoreCase(brandImport.getName()).isPresent()) {
					throw new ValidationException(messageByLocaleService.getMessage("brand.name.not.unique", null));
				} else {
					final BrandDTO brandDTO = new BrandDTO();
					brandDTO.setName(brandImport.getName());
					brandDTO.setActive(true);
					addBrand(brandDTO);
					brandImport.setUploadMessage(messageByLocaleService.getMessage("upload.success", null));
				}
			} catch (Exception e) {
				brandImport.setUploadMessage(messageByLocaleService.getMessage("upload.failure", new Object[] { e.getMessage() }));
			}
			allResult.add(brandImport);
		}
		return allResult;
	}

	@Override
	public List<Brand> getBrandList(final Boolean activeRecords, final String searchKeyword) {
		if (activeRecords != null) {
			if (searchKeyword != null) {
				return brandRepository.findAllByActiveAndNameContainingIgnoreCase(activeRecords, searchKeyword);
			} else {
				return brandRepository.findAllByActive(activeRecords);
			}
		} else {
			if (searchKeyword != null) {
				return brandRepository.findAllByNameContainingIgnoreCase(searchKeyword);
			} else {
				return brandRepository.findAll();
			}
		}
	}
}
