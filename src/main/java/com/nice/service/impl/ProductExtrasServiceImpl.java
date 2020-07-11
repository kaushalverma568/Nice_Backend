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
import com.nice.model.UserLogin;
import com.nice.repository.ProductExtrasRepository;
import com.nice.service.ProductExtrasService;
import com.nice.service.ProductService;

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

	@Override
	public Long addProductExtras(final ProductExtrasDTO productExtrasDTO) throws NotFoundException, ValidationException {
		Long vendorId = getVendorIdForLoginUser();
		Product product = productService.getProductDetail(productExtrasDTO.getProductId());
		if (!product.getVendorId().equals(vendorId)) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		ProductExtras productExtras = productExtrasMapper.toEntity(productExtrasDTO);

		productExtras.setProduct(product);
		productExtras.setVendorId(vendorId);
		productExtras = productExtrasRepository.save(productExtras);
		return productExtras.getId();
	}

	@Override
	public Long updateProductExtras(final ProductExtrasDTO productExtrasDTO) throws NotFoundException, ValidationException {
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
		productExtras.setVendorId(vendorId);
		productExtras.setProduct(prdoduct);
		productExtrasRepository.save(productExtras);
		return productExtras.getId();
	}

	@Override
	public ProductExtrasDTO getProductExtras(final Long productExtrasId) throws NotFoundException {
		ProductExtras productExtras = getProductExtrasDetail(productExtrasId);
		return productExtrasMapper.toDto(productExtras);
	}

	@Override
	public void changeStatus(final Long productExtrasId, final Boolean active) throws ValidationException, NotFoundException {
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
				throw new ValidationException(messageByLocaleService.getMessage("product.attribute.active", null));
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("product.attribute.deactive", null));
			}
		} else {
			existingProductExtras.setActive(active);
			productExtrasRepository.save(existingProductExtras);
		}
	}

	@Override
	public List<ProductExtrasDTO> getListWithUserCheck(final Long productId, Boolean activeRecords) throws NotFoundException, ValidationException {
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
		Product product = productService.getProductDetail(productId);
		List<ProductExtras> productExtraList;
		if (activeRecords != null) {
			productExtraList = productExtrasRepository.findAllByProductAndActive(product, activeRecords);
		} else {
			productExtraList = productExtrasRepository.findAllByProduct(product);
		}
		return productExtrasMapper.toDtos(productExtraList);
	}

	@Override
	public boolean isExists(final ProductExtrasDTO productExtrasDTO) throws NotFoundException {
		Product product = productService.getProductDetail(productExtrasDTO.getProductId());
		if (productExtrasDTO.getId() != null) {
			return productExtrasRepository.findByNameIgnoreCaseAndProductAndIdNot(productExtrasDTO.getName(), product, productExtrasDTO.getId()).isPresent();

		} else {
			return productExtrasRepository.findByNameIgnoreCaseAndProduct(productExtrasDTO.getName(), product).isPresent();
		}
	}

	@Override
	public ProductExtras getProductExtrasDetail(final Long productExtrasId) throws NotFoundException {
		return productExtrasRepository.findById(productExtrasId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] { productExtrasId })));
	}

	@Override
	public void deleteProductExtras(final Long productExtrasId) {
		productExtrasRepository.deleteById(productExtrasId);
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
