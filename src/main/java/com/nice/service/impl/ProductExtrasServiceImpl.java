package com.nice.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

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
	public ProductExtrasDTO addProductExtras(final ProductExtrasDTO productExtrasDTO) throws NotFoundException {
		Long vendorId = getVendorIdForLoginUser();
		ProductExtras productExtras = productExtrasMapper.toEntity(productExtrasDTO);
		Product product = productService.getProductDetail(productExtrasDTO.getProductId());
		productExtras.setProduct(product);
		productExtras.setVendorId(vendorId);
		productExtras = productExtrasRepository.save(productExtras);
		return productExtrasMapper.toDto(productExtras);
	}

	@Override
	public ProductExtrasDTO updateProductExtras(final ProductExtrasDTO productExtrasDTO) throws NotFoundException, ValidationException {
		Long vendorId = getVendorIdForLoginUser();

		if (productExtrasDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("product.extras.id.not.null", null));
		}
		Optional<ProductExtras> optExistingProductExtras = productExtrasRepository.findById(productExtrasDTO.getId());
		if (!optExistingProductExtras.isPresent()) {
			LOGGER.error("ProductExtras is not exists for ProductExtrasId {} ", productExtrasDTO.getId());
			throw new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] { productExtrasDTO.getId() }));
		} else if (!optExistingProductExtras.get().getVendorId().equals(vendorId)) {
			throw new NotFoundException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		ProductExtras productExtras = productExtrasMapper.toEntity(productExtrasDTO);
		Product prdoduct = productService.getProductDetail(productExtrasDTO.getProductId());
		productExtras.setVendorId(vendorId);
		productExtras.setProduct(prdoduct);
		return productExtrasMapper.toDto(productExtrasRepository.save(productExtras));
	}

	@Override
	public ProductExtrasDTO getProductExtras(final Long ProductExtrasId) throws NotFoundException {
		ProductExtras productExtras = productExtrasRepository.findById(ProductExtrasId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] { ProductExtrasId })));
		return productExtrasMapper.toDto(productExtras);
	}

	@Override
	public void changeStatus(final Long productExtrasId, final Boolean active) throws ValidationException, NotFoundException {
		UserLogin userLogin = getUserLoginFromToken();
		Long vendorId = getVendorIdForLoginUser();
		ProductExtras existingProductExtras = productExtrasRepository.findById(productExtrasId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] { productExtrasId })));
		if (!(userLogin.getEntityType() == null || existingProductExtras.getVendorId().equals(vendorId))) {
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
	public List<ProductExtrasDTO> getList(final Long productId, final Boolean activeRecords) throws NotFoundException, ValidationException {
		Product product = productService.getProductDetail(productId);
		List<ProductExtras> productExtraList = new ArrayList<>();
		UserLogin userLogin = getUserLoginFromToken();
		if (UserType.VENDOR.name().equals(userLogin.getEntityType()) && !product.getVendorId().equals(userLogin.getEntityId())) {
			throw new ValidationException(Constant.UNAUTHORIZED, null);
		}
		if (activeRecords != null) {
			productExtraList = productExtrasRepository.findAllByProductAndActive(product, activeRecords);
		} else {
			productExtraList = productExtrasRepository.findAllByProduct(product);
		}
		return productExtrasMapper.toDtos(productExtraList);
	}

	@Override
	public boolean isExists(final ProductExtrasDTO productExtrasDTO) throws ValidationException {
		Long vendorId = getVendorIdForLoginUser();
		if (vendorId == null) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		if (productExtrasDTO.getId() != null) {
			return !(productExtrasRepository.findByNameIgnoreCaseAndVendorIdAndIdNot(productExtrasDTO.getName(), vendorId, productExtrasDTO.getId()).isEmpty());

		} else {
			return !(productExtrasRepository.findByNameIgnoreCaseAndVendorId(productExtrasDTO.getName(), vendorId).isEmpty());
		}
	}

	@Override
	public ProductExtras getProductExtrasDetail(final Long ProductExtrasId) throws NotFoundException {
		return productExtrasRepository.findById(ProductExtrasId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] { ProductExtrasId })));
	}

	@Override
	public void deleteProductExtras(final Long productExtrasId) {
		productExtrasRepository.deleteById(productExtrasId);
	}

	private UserLogin getUserLoginFromToken() {
		return ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
	}

	/**
	 *
	 */
	private Long getVendorIdForLoginUser() {
		UserLogin userLogin = getUserLoginFromToken();
		if (UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			return userLogin.getEntityId();
		}
		return null;
	}

}
