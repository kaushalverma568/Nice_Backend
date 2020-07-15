package com.nice.service.impl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.Constant;
import com.nice.constant.UserType;
import com.nice.dto.ProductExtrasMasterDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductExtrasMasterMapper;
import com.nice.model.Product;
import com.nice.model.ProductExtras;
import com.nice.model.ProductExtrasMaster;
import com.nice.model.UserLogin;
import com.nice.repository.ProductExtrasMasterRepository;
import com.nice.repository.ProductExtrasRepository;
import com.nice.service.ProductExtrasMasterService;
import com.nice.service.ProductExtrasService;
import com.nice.service.ProductService;

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
	private ProductService productService;
	
	@Autowired
	private ProductExtrasService productExtrasService;
	
	@Autowired
	private ProductExtrasRepository productExtrasRepository;

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


	public List<ProductExtrasMasterDTO> getListWithUserCheck(final Long productId, Boolean activeRecords) throws NotFoundException, ValidationException {
		LOGGER.info("Inside changeStatus method, with productId : {} and active :{}", productId, activeRecords);
		Product product = productService.getProductDetail(productId);
		UserLogin userLogin = getUserLoginFromToken();
		/**
		 * If the userLogin is null or userType is customer show only activeRecords irrespective of what is sent from front end.
		 */
		if (userLogin != null && (UserType.VENDOR.name().equals(userLogin.getEntityType()) && !product.getVendorId().equals(userLogin.getEntityId()))) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		} else if (userLogin == null || UserType.CUSTOMER.name().equals(userLogin.getEntityType())) {
			activeRecords = true;
		}
		return getList(activeRecords, productId);
	}

	/**
	 * @param activeRecords
	 * @param product
	 * @return
	 * @throws NotFoundException
	 */
	@Override
	public List<ProductExtrasMasterDTO> getList(final Boolean activeRecords, final Long vendorId) throws NotFoundException {
		LOGGER.info("Inside getList method, with productId : {} and active :{}", vendorId, activeRecords);
		List<ProductExtrasMaster> productExtraList;
		if (activeRecords != null) {
			productExtraList = productExtrasMasterRepository.findAllByActiveAndVendorId(activeRecords, vendorId);
		} else {
			productExtraList = productExtrasMasterRepository.findAllByVendorId(vendorId);
		}
		LOGGER.info("After getList method, with productId : {} and active :{}", vendorId, activeRecords);
		return productExtrasMasterMapper.toDtos(productExtraList);
	}

	@Override
	public boolean isExists(final ProductExtrasMasterDTO productExtrasMasterDTO) {
		LOGGER.info("Inside isExists method, with productExtrasMasterDTO : {} ", productExtrasMasterDTO);
		if (productExtrasMasterDTO.getId() != null) {
			return productExtrasMasterRepository.findByNameIgnoreCaseAndIdNot(productExtrasMasterDTO.getName(), productExtrasMasterDTO.getId()).isPresent();

		} else {
			return productExtrasMasterRepository.findByNameIgnoreCase(productExtrasMasterDTO.getName()).isPresent();
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

}
