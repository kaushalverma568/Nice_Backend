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
import com.nice.dto.ProductExtrasDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductExtrasMapper;
import com.nice.model.Product;
import com.nice.model.ProductExtras;
import com.nice.model.ProductExtrasMaster;
import com.nice.model.UserLogin;
import com.nice.repository.ProductExtrasRepository;
import com.nice.service.CartExtrasService;
import com.nice.service.ProductExtrasMasterService;
import com.nice.service.ProductExtrasService;
import com.nice.service.ProductService;
import com.nice.service.TempCartExtrasService;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Service
@Transactional(rollbackFor = Throwable.class)
public class ProductExtrasServiceImpl implements ProductExtrasService {

	/**
	 *
	 */

	private static final Logger LOGGER = LoggerFactory.getLogger(ProductExtrasServiceImpl.class);

	private static final String NOT_FOUND = "product.extras.not.found";

	@Autowired
	private ProductExtrasRepository productExtrasRepository;

	@Autowired
	private ProductExtrasMapper productExtrasMapper;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private ProductService productService;
	
	@Autowired
	private CartExtrasService cartExtrasService;
	
	@Autowired
	private TempCartExtrasService tempCartExtrasService;
	
	@Autowired
	private ProductExtrasMasterService productExtrasMasterService;

	@Override
	public Long addProductExtras(final ProductExtrasDTO productExtrasDTO) throws NotFoundException, ValidationException {
		LOGGER.info("Inside addProductExtras method, with productExtrasDTO : {}", productExtrasDTO);
		Long vendorId = getVendorIdForLoginUser();
		Product product = productService.getProductDetail(productExtrasDTO.getProductId());
		ProductExtrasMaster extrasMaster = productExtrasMasterService.getProductExtrasMasterDetail(productExtrasDTO.getProductExtrasMasterId());
		if (!product.getVendorId().equals(vendorId)) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		ProductExtras productExtras = productExtrasMapper.toEntity(productExtrasDTO);

		productExtras.setProductExtrasMaster(extrasMaster);
		productExtras.setRate(extrasMaster.getRate());
		productExtras.setProduct(product);
		productExtras.setVendorId(vendorId);
		productExtras = productExtrasRepository.save(productExtras);
		LOGGER.info("After addProductExtras method");
		return productExtras.getId();
	}

	@Override
	public Long updateProductExtras(final ProductExtrasDTO productExtrasDTO) throws NotFoundException, ValidationException {
		LOGGER.info("Inside updateProductExtras method, with productExtrasDTO : {}", productExtrasDTO);
		Long vendorId = getVendorIdForLoginUser();

		if (productExtrasDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("product.extras.id.not.null", null));
		}
		ProductExtras productExtras = getProductExtrasDetail(productExtrasDTO.getId());
		if (!productExtras.getVendorId().equals(vendorId)) {
			throw new NotFoundException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		productExtras = productExtrasMapper.toEntity(productExtrasDTO);
		Product prdoduct = productService.getProductDetail(productExtrasDTO.getProductId());
		ProductExtrasMaster extrasMaster = productExtrasMasterService.getProductExtrasMasterDetail(productExtrasDTO.getProductExtrasMasterId());
		productExtras.setProductExtrasMaster(extrasMaster);
		productExtras.setRate(extrasMaster.getRate());
		productExtras.setVendorId(vendorId);
		productExtras.setProduct(prdoduct);
		productExtrasRepository.save(productExtras);
		LOGGER.info("After updateProductExtras method");
		return productExtras.getId();
	}

	@Override
	public ProductExtrasDTO getProductExtras(final Long productExtrasId) throws NotFoundException {
		LOGGER.info("Inside updateProductExtras method, with productExtrasId : {}", productExtrasId);
		ProductExtras productExtras = getProductExtrasDetail(productExtrasId);
		return productExtrasMapper.toDto(productExtras);
	}

	@Override
	public void changeStatus(final Long productExtrasId, final Boolean active) throws ValidationException, NotFoundException {
		LOGGER.info("Inside changeStatus method, with productExtrasId : {} and active :{}", productExtrasId, active);
		Long vendorId = getVendorIdForLoginUser();
		ProductExtras existingProductExtras = getProductExtrasDetail(productExtrasId);
		if (!existingProductExtras.getVendorId().equals(vendorId)) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		LOGGER.info("Existing  ProductExtras details {} ", existingProductExtras);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingProductExtras.getActive().equals(active)) {
			if (active) {
				throw new ValidationException(messageByLocaleService.getMessage("product.extras.active", null));
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("product.extras.deactive", null));
			}
		} else {
			if (Boolean.FALSE.equals(active)) {
				//delete from cart and temp cart 
                 tempCartExtrasService.deleteTempCartExtrasByExtrasId(productExtrasId);
                 cartExtrasService.deleteCartExtrasByExtrasId(productExtrasId);
			} else {
				if (Boolean.FALSE.equals(existingProductExtras.getProductExtrasMaster().getActive())) {
					throw new ValidationException(messageByLocaleService.getMessage("product.extras.master.activate.first", null));
				}
			}
			existingProductExtras.setActive(active);
			productExtrasRepository.save(existingProductExtras);
		}
		LOGGER.info("Inside changeStatus method, with productExtrasId : {} and active :{}", productExtrasId, active);
	}

	@Override
	public List<ProductExtrasDTO> getListWithUserCheck(final Long productId, Boolean activeRecords) throws NotFoundException, ValidationException {
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
	public List<ProductExtrasDTO> getList(final Boolean activeRecords, final Long productId) throws NotFoundException {
		LOGGER.info("Inside getList method, with productId : {} and active :{}", productId, activeRecords);
		Product product = productService.getProductDetail(productId);
		List<ProductExtras> productExtraList;
		if (activeRecords != null) {
			productExtraList = productExtrasRepository.findAllByProductAndActive(product, activeRecords);
		} else {
			productExtraList = productExtrasRepository.findAllByProduct(product);
		}
		LOGGER.info("After getList method, with productId : {} and active :{}", productId, activeRecords);
		return productExtrasMapper.toDtos(productExtraList);
	}
	
	
	@Override
	public List<ProductExtras> getListByProductExtrasMaster(final Boolean activeRecords, final Long productExtrasMasterId) throws NotFoundException {
		LOGGER.info("Inside getList method, with productId : {} and active :{}", productExtrasMasterId, activeRecords);
		ProductExtrasMaster productExtrasMaster = productExtrasMasterService.getProductExtrasMasterDetail(productExtrasMasterId);
		List<ProductExtras> productExtraList;
		if (activeRecords != null) {
			productExtraList = productExtrasRepository.findAllByProductExtrasMasterAndActive(productExtrasMaster, activeRecords);
		} else {
			productExtraList = productExtrasRepository.findAllByProductExtrasMaster(productExtrasMaster);
		}
		LOGGER.info("After getList method, with productId : {} and active :{}", productExtrasMasterId, activeRecords);
		return productExtraList;
	}

	@Override
	public boolean isExists(final ProductExtrasDTO productExtrasDTO) throws NotFoundException {
		LOGGER.info("Inside isExists method, with productExtrasDTO : {} ", productExtrasDTO);
		Product product = productService.getProductDetail(productExtrasDTO.getProductId());
		ProductExtrasMaster extrasMaster = productExtrasMasterService.getProductExtrasMasterDetail(productExtrasDTO.getProductExtrasMasterId());
		if (productExtrasDTO.getId() != null) {
			return productExtrasRepository.findByProductAndProductExtrasMasterAndIdNot(product, extrasMaster, productExtrasDTO.getId()).isPresent();

		} else {
			return productExtrasRepository.findByProductAndProductExtrasMaster(product, extrasMaster).isPresent();
		}
	}

	@Override
	public ProductExtras getProductExtrasDetail(final Long productExtrasId) throws NotFoundException {
		LOGGER.info("Inside getProductExtrasDetail method, with productExtrasId : {} ", productExtrasId);
		return productExtrasRepository.findById(productExtrasId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] { productExtrasId })));
	}

	@Override
	public void deleteProductExtras(final Long productExtrasId) {
		LOGGER.info("Inside deleteProductExtras method, with productExtrasId : {} ", productExtrasId);
		productExtrasRepository.deleteById(productExtrasId);
		LOGGER.info("After deleteProductExtras method, with productExtrasId : {} ", productExtrasId);
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
