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
import com.nice.dto.ToppingDTO;
import com.nice.dto.ToppingImport;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ToppingMapper;
import com.nice.model.ProductTopping;
import com.nice.model.Topping;
import com.nice.model.UserLogin;
import com.nice.model.Vendor;
import com.nice.repository.ToppingRepository;
import com.nice.service.FileStorageService;
import com.nice.service.ProductToppingService;
import com.nice.service.ToppingService;
import com.nice.service.VendorService;
import com.nice.util.CSVProcessor;
import com.nice.util.CommonUtility;
import com.nice.util.ExportCSV;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("toppingService")
public class ToppingServiceImpl implements ToppingService {

	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(ToppingServiceImpl.class);

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ToppingRepository toppingRepository;

	@Autowired
	private ProductToppingService productToppingService;

	@Autowired
	private ToppingMapper toppingMapper;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private ExportCSV exportCSV;

	@Autowired
	private FileStorageService fileStorageService;

	@Override
	public void addTopping(final ToppingDTO toppingDTO) throws ValidationException, NotFoundException {
		final Topping topping = toppingMapper.toEntity(toppingDTO);
		toppingRepository.save(topping);
	}

	@Override
	public void updateTopping(final ToppingDTO resultToppingDTO) throws NotFoundException, ValidationException {
		if (resultToppingDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("topping.id.not.null", null));
		} else {
			final Topping existingTopping = getToppingDetail(resultToppingDTO.getId());
			if (!existingTopping.getVendorId().equals(resultToppingDTO.getVendorId())) {
				throw new ValidationException(messageByLocaleService.getMessage("vendor.id.not.unique", null));
			}
			final Topping topping = toppingMapper.toEntity(resultToppingDTO);
			topping.setVendorId(existingTopping.getVendorId());
			toppingRepository.save(topping);
		}
	}

	@Override
	public ToppingDTO getTopping(final Long toppingId) throws NotFoundException, ValidationException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		final Topping existingTopping = getToppingDetail(toppingId);
		if (!existingTopping.getVendorId().equals(userLogin.getEntityId())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		return toppingMapper.toDto(existingTopping);
	}

	@Override
	public Topping getToppingDetail(final Long toppingId) throws NotFoundException {
		return toppingRepository.findById(toppingId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("topping.not.found", new Object[] { toppingId })));
	}

	@Override
	public Page<Topping> getToppingList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final Long vendorId) {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("name"));
		if (activeRecords != null) {
			if (vendorId != null) {
				return toppingRepository.findAllByActiveAndVendorId(activeRecords, vendorId, pageable);
			} else {
				return toppingRepository.findAllByActive(activeRecords, pageable);
			}
		} else {
			if (vendorId != null) {
				return toppingRepository.findAllByVendorId(vendorId, pageable);
			} else {
				return toppingRepository.findAll(pageable);
			}
		}
	}

	@Override
	public void changeStatus(final Long toppingId, final Boolean active) throws NotFoundException, ValidationException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		final Topping existingTopping = getToppingDetail(toppingId);
		LOGGER.info("Existing topping details {} ", existingTopping);
		if (!userLogin.getEntityId().equals(existingTopping.getVendorId())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		} else if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingTopping.getActive().equals(active)) {
			throw new ValidationException(messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "topping.active" : "topping.deactive", null));
		} else {
			/**
			 * deActive All product toppings related to this topping at the time of
			 * deActivating
			 */
			if (Boolean.FALSE.equals(active)) {
				LOGGER.info("DeActivating  Topping {}", existingTopping);
				List<ProductTopping> productToppingList = productToppingService.getProductToppingListForTopping(existingTopping, Boolean.TRUE);
				for (final ProductTopping productTopping : productToppingList) {
					productToppingService.changeStatus(productTopping.getId(), Boolean.FALSE);
				}
			} else {
				LOGGER.info("Activating  Topping");
			}
			existingTopping.setActive(active);
			toppingRepository.save(existingTopping);
		}
	}

	@Override
	public Boolean isToppingExists(final ToppingDTO toppingDTO) {
		if (toppingDTO.getId() != null) {
			/**
			 * At the time of update is topping with same name and vendor id exist or not
			 * except it's own id
			 */
			return toppingRepository.findByNameIgnoreCaseAndVendorIdAndIdNot(toppingDTO.getName(), toppingDTO.getVendorId(), toppingDTO.getId()).isPresent();
		} else {
			/**
			 * At the time of create is topping with same name and vendor id exist or not
			 */
			return toppingRepository.findByNameIgnoreCaseAndVendorId(toppingDTO.getName(), toppingDTO.getVendorId()).isPresent();
		}
	}

	@Override
	public void uploadFile(final MultipartFile multipartFile, final HttpServletResponse httpServletResponse) throws FileOperationException {
		final String fileName = fileStorageService.storeFile(multipartFile, "topping_" + System.currentTimeMillis(), AssetConstant.TOPPING);
		Path filePath = fileStorageService.getOriginalFilePath(fileName, AssetConstant.TOPPING);
		final File file = new File(filePath.toString());
		final CSVProcessor<ToppingImport> csvProcessor = new CSVProcessor<>();
		try {
			final List<ToppingImport> toppingImports = csvProcessor.convertCSVFileToListOfBean(file, ToppingImport.class);
			if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(toppingImports)) {
				final List<ToppingImport> insertListOfBean = insertListOfUoms(
						toppingImports.stream().filter(x -> CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(x.getName())).collect(Collectors.toList()));
				Object[] toppingDetailsHeadersField = new Object[] { "Topping Name", "Description", "Product Food type", "Result" };
				Object[] toppingDetailsField = new Object[] { "name", "description", "productFoodType", "uploadMessage" };
				exportCSV.writeCSVFile(insertListOfBean, toppingDetailsField, toppingDetailsHeadersField, httpServletResponse);
			}
		} catch (SecurityException | IOException e) {
			throw new FileOperationException(messageByLocaleService.getMessage("import.file.error", null));
		}

	}

	private List<ToppingImport> insertListOfUoms(final List<ToppingImport> toppingImports) {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		final List<ToppingImport> allResult = new ArrayList<>();
		for (ToppingImport toppingImport : toppingImports) {
			try {
				if (!UserType.VENDOR.name().equals(userLogin.getEntityType())) {
					throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
				}
				Vendor vendor = vendorService.getVendorDetail(userLogin.getEntityId());
				if (toppingRepository.findByNameIgnoreCaseAndVendorId(toppingImport.getName(), vendor.getId()).isPresent()) {
					throw new ValidationException(messageByLocaleService.getMessage("topping.not.unique", null));
				} else {
					final ToppingDTO toppingDTO = new ToppingDTO();
					toppingDTO.setName(toppingImport.getName());
					toppingDTO.setDescription(toppingImport.getDescription());
					toppingDTO.setProductFoodType(toppingImport.getProductFoodType());
					toppingDTO.setActive(true);
					addTopping(toppingDTO);
					toppingImport.setUploadMessage(messageByLocaleService.getMessage("upload.success", null));
				}
			} catch (Exception e) {
				toppingImport.setUploadMessage(messageByLocaleService.getMessage("upload.failure", new Object[] { e.getMessage() }));
			}
			allResult.add(toppingImport);
		}
		return allResult;
	}
}
