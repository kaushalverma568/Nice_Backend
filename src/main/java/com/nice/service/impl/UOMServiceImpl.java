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
import com.nice.dto.ProductParamRequestDTO;
import com.nice.dto.UOMDTO;
import com.nice.dto.UOMImport;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.UOMMapper;
import com.nice.model.Product;
import com.nice.model.UOM;
import com.nice.model.UserLogin;
import com.nice.model.Vendor;
import com.nice.repository.UOMRepository;
import com.nice.service.FileStorageService;
import com.nice.service.ProductService;
import com.nice.service.UOMService;
import com.nice.service.VendorService;
import com.nice.util.CSVProcessor;
import com.nice.util.CommonUtility;
import com.nice.util.ExportCSV;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("uomService")
public class UOMServiceImpl implements UOMService {

	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(UOMServiceImpl.class);

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private UOMRepository uomRepository;

	@Autowired
	private UOMMapper uomMapper;

	@Autowired
	private ProductService productService;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private ExportCSV exportCSV;

	@Autowired
	private FileStorageService fileStorageService;

	@Override
	public void addUOM(final UOMDTO uomDTO) throws ValidationException, NotFoundException {
		validateDto(uomDTO);
		UOM uom = uomMapper.toEntity(uomDTO);
		StringBuilder label = new StringBuilder();
		if (uom.getQuantity() % 1 == 0) {
			label.append(uom.getQuantity().intValue());
		} else {
			label.append(uom.getQuantity());
		}

		uom.setUomLabelEnglish(label.toString().concat(" " + uom.getMeasurementEnglish()));
		uom.setUomLabelArabic(label.toString().concat(" " + uom.getMeasurementArabic()));
		uomRepository.save(uom);
	}

	private void validateDto(final UOMDTO uomDTO) throws ValidationException {
		/**
		 * If the login user is not a vendor then throw an exception
		 */
		UserLogin userLogin = checkForUserLogin();
		if (!UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		if (!uomDTO.getVendorId().equals(userLogin.getEntityId())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}

	}

	@Override
	public void updateUOM(final UOMDTO resultUOMDTO) throws NotFoundException, ValidationException {
		validateDto(resultUOMDTO);
		if (resultUOMDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("uom.id.not.null", null));
		} else {
			getUOMDetail(resultUOMDTO.getId());
			UOM uom = uomMapper.toEntity(resultUOMDTO);
			StringBuilder label = new StringBuilder();
			if (uom.getQuantity() % 1 == 0) {
				label.append(uom.getQuantity().intValue());
			} else {
				label.append(uom.getQuantity());
			}
			uom.setUomLabelEnglish(label.toString().concat(" " + uom.getMeasurementEnglish()));
			uom.setUomLabelArabic(label.toString().concat(" " + uom.getMeasurementArabic()).toString());
			uomRepository.save(uom);
		}
	}

	@Override
	public UOMDTO getUOM(final Long uomId) throws NotFoundException {
		final UOM existingUOM = getUOMDetail(uomId);
		return uomMapper.toDto(existingUOM);
	}

	@Override
	public UOM getUOMDetail(final Long uomId) throws NotFoundException {
		return uomRepository.findById(uomId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("uom.not.found", new Object[] { uomId })));
	}

	@Override
	public Page<UOM> getUOMList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final Long vendorId) throws NotFoundException {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("id"));
		if (activeRecords != null) {
			if (vendorId != null) {
				return uomRepository.findAllByActiveAndVendorId(activeRecords, vendorId, pageable);
			} else {
				return uomRepository.findAllByActive(activeRecords, pageable);
			}
		} else {
			if (vendorId != null) {
				return uomRepository.findAllByVendorId(vendorId, pageable);
			} else {
				return uomRepository.findAll(pageable);
			}

		}
	}

	@Override
	public void changeStatus(final Long uomId, final Boolean active) throws NotFoundException, ValidationException {
		final UOM existingUOM = getUOMDetail(uomId);
		LOGGER.info("Existing uom details {} ", existingUOM);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingUOM.getActive().equals(active)) {
			if (Boolean.TRUE.equals(active)) {
				throw new ValidationException(messageByLocaleService.getMessage("uom.already.active", null));
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("uom.already.deactive", null));
			}
		} else {
			/**
			 * deActive UOM only if there is no product which is contain uom
			 */
			if (Boolean.FALSE.equals(active)) {
				final ProductParamRequestDTO paramRequestDTO = new ProductParamRequestDTO();
				paramRequestDTO.setUomId(uomId);
				final List<Product> productList = productService.getProductListBasedOnParamsWithoutPagination(paramRequestDTO);
				if (!productList.isEmpty()) {
					throw new ValidationException(messageByLocaleService.getMessage("uom.can.not.deactive", null));
				}
			} else {
				LOGGER.info("Activating  UOM");
			}
			existingUOM.setActive(active);
			uomRepository.save(existingUOM);
		}
	}

	@Override
	public Boolean isUOMExistsEnglish(final UOMDTO uomDTO) {
		if (uomDTO.getId() != null) {
			/**
			 * At the time of update is uom with same measurement exist or not except it's own id
			 */
			return uomRepository.findByMeasurementEnglishIgnoreCaseAndQuantityAndVendorIdAndIdNot(uomDTO.getMeasurementEnglish(), uomDTO.getQuantity(),
					uomDTO.getVendorId(), uomDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is uom with same measurement exist or not
			 */
			return uomRepository
					.findByMeasurementEnglishIgnoreCaseAndQuantityAndVendorId(uomDTO.getMeasurementEnglish(), uomDTO.getQuantity(), uomDTO.getVendorId())
					.isPresent();
		}
	}

	@Override
	public Boolean isUOMExistsArabic(final UOMDTO uomDTO) {
		if (uomDTO.getId() != null) {
			/**
			 * At the time of update is uom with same measurement exist or not except it's own id
			 */
			return uomRepository.findByMeasurementArabicIgnoreCaseAndQuantityAndVendorIdAndIdNot(uomDTO.getMeasurementArabic(), uomDTO.getQuantity(),
					uomDTO.getVendorId(), uomDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is uom with same measurement exist or not
			 */
			return uomRepository
					.findByMeasurementArabicIgnoreCaseAndQuantityAndVendorId(uomDTO.getMeasurementArabic(), uomDTO.getQuantity(), uomDTO.getVendorId())
					.isPresent();
		}
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

	@Override
	public void exportList(final Long vendorId, final Boolean activeRecords, final HttpServletResponse httpServletResponse) throws FileNotFoundException {
		List<UOM> uomList = null;
		List<UOMDTO> uomExportList = new ArrayList<>();
		if (activeRecords != null) {
			if (vendorId != null) {
				uomList = uomRepository.findAllByActiveAndVendorId(activeRecords, vendorId);
			} else {
				uomList = uomRepository.findAllByActive(activeRecords);
			}
		} else {
			if (vendorId != null) {
				uomList = uomRepository.findAllByVendorId(vendorId);
			} else {
				uomList = uomRepository.findAll();
			}
		}
		for (UOM uom : uomList) {
			uomExportList.add(uomMapper.toDto(uom));
		}
		final Object[] uomHeaderField = new Object[] { "Measurement English", "Measurement Arabic", "Quantity", "UOM Label English", "UOM Label Arabic" };
		final Object[] uomDataField = new Object[] { "measurementEnglish", "measurementArabic", "quantity", "uomLabelEnglish", "uomLabelArabic" };
		try {
			exportCSV.writeCSVFile(uomExportList, uomDataField, uomHeaderField, httpServletResponse);
		} catch (IOException e) {
			throw new FileNotFoundException(messageByLocaleService.getMessage("export.file.create.error", null));
		}
	}

	@Override
	public void uploadFile(final MultipartFile multipartFile, final HttpServletResponse httpServletResponse) throws FileOperationException {
		final String fileName = fileStorageService.storeFile(multipartFile, "uom", AssetConstant.UOM);
		Path filePath = fileStorageService.getOriginalFilePath(fileName, AssetConstant.UOM);
		final File file = new File(filePath.toString());
		final CSVProcessor<UOMImport> csvProcessor = new CSVProcessor<>();
		try {
			final List<UOMImport> uomImports = csvProcessor.convertCSVFileToListOfBean(file, UOMImport.class);
			if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(uomImports)) {
				final List<UOMImport> insertListOfBean = insertListOfUoms(
						uomImports.stream().filter(x -> CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(x.getMeasurementEnglish())
								&& CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(x.getMeasurementArabic())).collect(Collectors.toList()));
				Object[] uomDetailsHeadersField = new Object[] { "UOM Measurement English", "UOM Measurement Arabic", "Quantity", "Result" };
				Object[] uomDetailsField = new Object[] { "measurementEnglish", "measurementArabic", "Quantity", "uploadMessage" };
				exportCSV.writeCSVFile(insertListOfBean, uomDetailsField, uomDetailsHeadersField, httpServletResponse);
			}
		} catch (SecurityException | IOException e) {
			throw new FileOperationException(messageByLocaleService.getMessage("import.file.error", null));
		}

	}

	private List<UOMImport> insertListOfUoms(final List<UOMImport> uomImports) {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		final List<UOMImport> allResult = new ArrayList<>();
		for (UOMImport uomImport : uomImports) {
			try {
				if (!UserType.VENDOR.name().equals(userLogin.getEntityType())) {
					throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
				}
				Vendor vendor = vendorService.getVendorDetail(userLogin.getEntityId());
				if (uomRepository.findByMeasurementEnglishIgnoreCaseAndQuantityAndVendorIdOrMeasurementArabicIgnoreCaseAndQuantityAndVendorId(
						uomImport.getMeasurementEnglish(), uomImport.getQuantity(), vendor.getId(), uomImport.getMeasurementArabic(), uomImport.getQuantity(),
						vendor.getId()).isPresent()) {
					throw new ValidationException(messageByLocaleService.getMessage("uom.not.unique", null));
				} else {
					final UOMDTO uomDTO = new UOMDTO();
					uomDTO.setMeasurementEnglish(uomImport.getMeasurementEnglish());
					uomDTO.setMeasurementArabic(uomImport.getMeasurementArabic());
					uomDTO.setQuantity(uomImport.getQuantity());
					uomDTO.setActive(true);
					uomDTO.setVendorId(vendor.getId());
					addUOM(uomDTO);
					uomImport.setUploadMessage(messageByLocaleService.getMessage("upload.success", null));
				}
			} catch (Exception e) {
				uomImport.setUploadMessage(messageByLocaleService.getMessage("upload.failure", new Object[] { e.getMessage() }));
			}
			allResult.add(uomImport);
		}
		return allResult;
	}

}
