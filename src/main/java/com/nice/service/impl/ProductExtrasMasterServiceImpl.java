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
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.AssetConstant;
import com.nice.constant.Constant;
import com.nice.constant.UserType;
import com.nice.dto.ExtrasImport;
import com.nice.dto.ProductExtrasMasterDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductExtrasMasterMapper;
import com.nice.model.ProductExtras;
import com.nice.model.ProductExtrasMaster;
import com.nice.model.UserLogin;
import com.nice.model.Vendor;
import com.nice.repository.ProductExtrasMasterRepository;
import com.nice.repository.ProductExtrasRepository;
import com.nice.service.FileStorageService;
import com.nice.service.ProductExtrasMasterService;
import com.nice.service.ProductExtrasService;
import com.nice.service.VendorService;
import com.nice.util.CSVProcessor;
import com.nice.util.CommonUtility;
import com.nice.util.ExportCSV;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Service
@Transactional(rollbackFor = Throwable.class)
public class ProductExtrasMasterServiceImpl implements ProductExtrasMasterService {

	/**
	 *
	 */

	private static final Logger LOGGER = LoggerFactory.getLogger(ProductExtrasMasterServiceImpl.class);

	private static final String NOT_FOUND = "product.extras.master.not.found";

	@Autowired
	private ProductExtrasMasterRepository productExtrasMasterRepository;

	@Autowired
	private ProductExtrasMasterMapper productExtrasMasterMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductExtrasService productExtrasService;

	@Autowired
	private ProductExtrasRepository productExtrasRepository;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private ExportCSV exportCSV;

	@Autowired
	private FileStorageService fileStorageService;

	@Override
	public Long addProductExtrasMaster(final ProductExtrasMasterDTO productExtrasMasterDTO) throws NotFoundException, ValidationException {
		LOGGER.info("Inside addProductExtrasMaster method, with productExtrasMasterDTO : {}", productExtrasMasterDTO);
		Long vendorId = getVendorIdForLoginUser();
		if (!productExtrasMasterDTO.getVendorId().equals(vendorId)) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		ProductExtrasMaster productExtrasMaster = productExtrasMasterMapper.toEntity(productExtrasMasterDTO);
		productExtrasMaster.setVendorId(vendorId);
		productExtrasMaster = productExtrasMasterRepository.save(productExtrasMaster);
		LOGGER.info("After addProductExtrasMaster method");
		return productExtrasMaster.getId();
	}

	@Override
	public Long updateProductExtrasMaster(final ProductExtrasMasterDTO productExtrasMasterDTO) throws NotFoundException, ValidationException {
		LOGGER.info("Inside updateProductExtrasMaster method, with productExtrasMasterDTO : {}", productExtrasMasterDTO);
		Long vendorId = getVendorIdForLoginUser();

		if (productExtrasMasterDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("product.extras.master.id.not.null", null));
		}
		ProductExtrasMaster productExtrasMaster = getProductExtrasMasterDetail(productExtrasMasterDTO.getId());
		if (!productExtrasMaster.getVendorId().equals(vendorId)) {
			throw new NotFoundException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		productExtrasMaster = productExtrasMasterMapper.toEntity(productExtrasMasterDTO);
		productExtrasMaster.setVendorId(vendorId);
		productExtrasMasterRepository.save(productExtrasMaster);
		LOGGER.info("After updateProductExtrasMaster method");
		/**
		 * update rate in product Extras
		 */
		for (ProductExtras productExtras : productExtrasService.getListByProductExtrasMaster(null, productExtrasMaster.getId())) {
			productExtras.setRate(productExtrasMaster.getRate());
			productExtrasRepository.save(productExtras);
		}
		return productExtrasMaster.getId();
	}

	@Override
	public ProductExtrasMasterDTO getProductExtrasMaster(final Long productExtrasMasterId) throws NotFoundException {
		LOGGER.info("Inside updateProductExtrasMaster method, with productExtrasMasterId : {}", productExtrasMasterId);
		ProductExtrasMaster productExtrasMaster = getProductExtrasMasterDetail(productExtrasMasterId);
		return productExtrasMasterMapper.toDto(productExtrasMaster);
	}

	@Override
	public void changeStatus(final Long productExtrasMasterId, final Boolean active) throws ValidationException, NotFoundException {
		LOGGER.info("Inside changeStatus method, with productExtrasMasterId : {} and active :{}", productExtrasMasterId, active);
		Long vendorId = getVendorIdForLoginUser();
		ProductExtrasMaster existingProductExtrasMaster = getProductExtrasMasterDetail(productExtrasMasterId);
		if (!existingProductExtrasMaster.getVendorId().equals(vendorId)) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		LOGGER.info("Existing  ProductExtrasMaster details {} ", existingProductExtrasMaster);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingProductExtrasMaster.getActive().equals(active)) {
			if (active) {
				throw new ValidationException(messageByLocaleService.getMessage("product.extras.master.active", null));
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("product.extras.master.deactive", null));
			}
		} else {
			if (Boolean.FALSE.equals(active)) {
				final List<ProductExtras> productExtrasList = productExtrasService.getListByProductExtrasMaster(true, productExtrasMasterId);
				for (ProductExtras productExtra : productExtrasList) {
					productExtrasService.changeStatus(productExtra.getId(), active);
				}
			}
			existingProductExtrasMaster.setActive(active);
			productExtrasMasterRepository.save(existingProductExtrasMaster);
		}
		LOGGER.info("Inside changeStatus method, with productExtrasMasterId : {} and active :{}", productExtrasMasterId, active);
	}

	/**
	 * @param activeRecords
	 * @param product
	 * @return
	 * @throws NotFoundException
	 */
	@Override
	public Page<ProductExtrasMaster> getList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final Long vendorId)
			throws NotFoundException {
		LOGGER.info("Inside getList method, with productId : {} and active :{}", vendorId, activeRecords);
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("nameEnglish"));
		if (activeRecords != null) {
			if (vendorId != null) {
				return productExtrasMasterRepository.findAllByActiveAndVendorId(activeRecords, vendorId, pageable);
			} else {
				return productExtrasMasterRepository.findAllByActive(activeRecords, pageable);
			}
		} else {
			if (vendorId != null) {
				return productExtrasMasterRepository.findAllByVendorId(vendorId, pageable);
			} else {
				return productExtrasMasterRepository.findAll(pageable);
			}
		}
	}

	@Override
	public boolean isExistsEnglish(final ProductExtrasMasterDTO productExtrasMasterDTO) {
		LOGGER.info("Inside isExists method, with productExtrasMasterDTO : {} ", productExtrasMasterDTO);
		if (productExtrasMasterDTO.getId() != null) {
			return productExtrasMasterRepository.findByNameEnglishIgnoreCaseAndIdNot(productExtrasMasterDTO.getName(), productExtrasMasterDTO.getId())
					.isPresent();

		} else {
			return productExtrasMasterRepository.findByNameEnglishIgnoreCase(productExtrasMasterDTO.getName()).isPresent();
		}
	}

	@Override
	public boolean isExistsArabic(final ProductExtrasMasterDTO productExtrasMasterDTO) {
		LOGGER.info("Inside isExists method, with productExtrasMasterDTO : {} ", productExtrasMasterDTO);
		if (productExtrasMasterDTO.getId() != null) {
			return productExtrasMasterRepository.findByNameEnglishIgnoreCaseAndIdNot(productExtrasMasterDTO.getName(), productExtrasMasterDTO.getId())
					.isPresent();

		} else {
			return productExtrasMasterRepository.findByNameEnglishIgnoreCase(productExtrasMasterDTO.getName()).isPresent();
		}
	}

	@Override
	public ProductExtrasMaster getProductExtrasMasterDetail(final Long productExtrasMasterId) throws NotFoundException {
		LOGGER.info("Inside getProductExtrasMasterDetail method, with productExtrasMasterId : {} ", productExtrasMasterId);
		return productExtrasMasterRepository.findById(productExtrasMasterId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] { productExtrasMasterId })));
	}

	@Override
	public void deleteProductExtrasMaster(final Long productExtrasMasterId) {
		LOGGER.info("Inside deleteProductExtrasMaster method, with productExtrasMasterId : {} ", productExtrasMasterId);
		productExtrasMasterRepository.deleteById(productExtrasMasterId);
		LOGGER.info("After deleteProductExtrasMaster method, with productExtrasMasterId : {} ", productExtrasMasterId);
	}

	private UserLogin getUserLoginFromToken() {
		Object principal = SecurityContextHolder.getContext().getAuthentication().getPrincipal();
		if (Constant.ANONYMOUS_USER.equals(principal)) {
			return null;
		}
		return ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
	}

	private UserLogin checkForUserLogin() throws ValidationException {
		UserLogin userLogin = getUserLoginFromToken();
		if (userLogin == null) {
			throw new ValidationException(messageByLocaleService.getMessage("login.first", null));
		} else {
			return userLogin;
		}
	}

	/**
	 * @throws ValidationException
	 *
	 */
	private Long getVendorIdForLoginUser() throws ValidationException {
		UserLogin userLogin = checkForUserLogin();
		if (!UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		} else {
			return userLogin.getEntityId();
		}
	}

	@Override
	public void uploadFile(final MultipartFile multipartFile, final HttpServletResponse httpServletResponse) throws FileOperationException {
		final String fileName = fileStorageService.storeFile(multipartFile, "productExtrasMaster_" + System.currentTimeMillis(), AssetConstant.EXTRAS);
		Path filePath = fileStorageService.getOriginalFilePath(fileName, AssetConstant.EXTRAS);
		final File file = new File(filePath.toString());
		final CSVProcessor<ExtrasImport> csvProcessor = new CSVProcessor<>();
		try {
			final List<ExtrasImport> extrasImports = csvProcessor.convertCSVFileToListOfBean(file, ExtrasImport.class);
			if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(extrasImports)) {
				final List<ExtrasImport> insertListOfBean = insertListOfUoms(extrasImports.stream()
						.filter(x -> CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(x.getNameEnglish()) && x.getRate() > 0).collect(Collectors.toList()));
				Object[] extrasDetailsHeadersField = new Object[] { "Name", "Description", "Rate", "Result" };
				Object[] extrasDetailsField = new Object[] { "Name", "description", "rate", "uploadMessage" };
				exportCSV.writeCSVFile(insertListOfBean, extrasDetailsField, extrasDetailsHeadersField, httpServletResponse);
			}
		} catch (SecurityException | IOException e) {
			throw new FileOperationException(messageByLocaleService.getMessage("import.file.error", null));
		}

	}

	private List<ExtrasImport> insertListOfUoms(final List<ExtrasImport> productExtrasMasterImports) {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		final List<ExtrasImport> allResult = new ArrayList<>();
		for (ExtrasImport productExtrasMasterImport : productExtrasMasterImports) {
			try {
				if (!UserType.VENDOR.name().equals(userLogin.getEntityType())) {
					throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
				}
				Vendor vendor = vendorService.getVendorDetail(userLogin.getEntityId());
				if (!productExtrasMasterRepository.findByNameEnglishIgnoreCaseAndVendorId(productExtrasMasterImport.getNameEnglish(), vendor.getId())
						.isEmpty()) {
					throw new ValidationException(messageByLocaleService.getMessage("productExtrasMaster.not.unique", null));
				} else if (!productExtrasMasterRepository.findByNameArabicIgnoreCaseAndVendorId(productExtrasMasterImport.getNameArabic(), vendor.getId())
						.isEmpty()) {
					throw new ValidationException(messageByLocaleService.getMessage("productExtrasMaster.not.unique", null));
				} else {
					final ProductExtrasMasterDTO productExtrasMasterDTO = new ProductExtrasMasterDTO();
					productExtrasMasterDTO.setNameEnglish(productExtrasMasterImport.getNameEnglish());
					productExtrasMasterDTO.setDescriptionEnglish(productExtrasMasterImport.getDescriptionEnglish());
					productExtrasMasterDTO.setNameArabic(productExtrasMasterImport.getNameArabic());
					productExtrasMasterDTO.setDescriptionArabic(productExtrasMasterImport.getDescriptionArabic());
					productExtrasMasterDTO.setRate(productExtrasMasterImport.getRate());
					productExtrasMasterDTO.setActive(true);
					productExtrasMasterDTO.setVendorId(vendor.getId());
					addProductExtrasMaster(productExtrasMasterDTO);
					productExtrasMasterImport.setUploadMessage(messageByLocaleService.getMessage("upload.success", null));
				}
			} catch (Exception e) {
				productExtrasMasterImport.setUploadMessage(messageByLocaleService.getMessage("upload.failure", new Object[] { e.getMessage() }));
			}
			allResult.add(productExtrasMasterImport);
		}
		return allResult;
	}

}
