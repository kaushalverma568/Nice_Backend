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
 *
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
		label.append(" " + uom.getMeasurement());
		uom.setUomLabel(label.toString());
		uomRepository.save(uom);
	}

	private void validateDto(UOMDTO uomDTO) throws ValidationException {
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
			label.append(" " + uom.getMeasurement());
			uom.setUomLabel(label.toString());
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
	public Page<UOM> getUOMList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, Long vendorId) throws NotFoundException {
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
	public Boolean isUOMExists(final UOMDTO uomDTO) {
		if (uomDTO.getId() != null) {
			/**
			 * At the time of update is uom with same measurement exist or not except it's own id
			 */
			return uomRepository.findByMeasurementIgnoreCaseAndQuantityAndIdNot(uomDTO.getMeasurement(), uomDTO.getQuantity(), uomDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is uom with same measurement exist or not
			 */
			return uomRepository.findByMeasurementIgnoreCaseAndQuantity(uomDTO.getMeasurement(), uomDTO.getQuantity()).isPresent();
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
	public void exportList(Boolean activeRecords, HttpServletResponse httpServletResponse) throws IOException {
		List<UOM> uomList;
		List<UOMDTO> uomExportList = new ArrayList<>();
		if (activeRecords != null) {
			uomList = uomRepository.findAllByActive(activeRecords);
		} else {
			uomList = uomRepository.findAll();
		}
		for (UOM uom : uomList) {
			uomExportList.add(uomMapper.toDto(uom));
		}
		final Object[] uomHeaderField = new Object[] {"Measurement","Quantity","UOM Label"};
		final Object[] uomDataField = new Object[] { "measurement","quantity","uomLabel"};
		exportCSV.writeCSVFile(uomExportList, uomDataField, uomHeaderField, httpServletResponse);		
	}

	@Override
	public void uploadFile(MultipartFile multipartFile, HttpServletResponse httpServletResponse) throws FileOperationException {
		final String fileName = fileStorageService.storeFile(multipartFile, "uom", AssetConstant.UOM);
		Path filePath = fileStorageService.getOriginalFilePath(fileName, AssetConstant.UOM);
		final File file = new File(filePath.toString());
		final CSVProcessor<UOMImport> csvProcessor = new CSVProcessor<>();
		try {
			final List<UOMImport> uomImports = csvProcessor.convertCSVFileToListOfBean(file, UOMImport.class);
			if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(uomImports)) {
				final List<UOMImport> insertListOfBean = insertListOfUoms(
						uomImports.stream().filter(x -> CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(x.getMeasurement())).collect(Collectors.toList()));
				Object[] uomDetailsHeadersField = new Object[] { "UOM Measurement","Quantity", "Result" };
				Object[] uomDetailsField = new Object[] { "measurement", "Quantity","uploadMessage" };
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
				if (uomRepository.findByMeasurementIgnoreCaseAndQuantityAndVendorId(uomImport.getMeasurement(),
						uomImport.getQuantity(), vendor.getId()).isPresent()) {
					throw new ValidationException(messageByLocaleService.getMessage("uom.not.unique", null));
				} else {
					final UOMDTO uomDTO = new UOMDTO();
					uomDTO.setMeasurement(uomImport.getMeasurement());
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
