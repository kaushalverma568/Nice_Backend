package com.nice.service.impl;

import java.util.List;
import java.util.Optional;

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

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.Constant;
import com.nice.constant.UserType;
import com.nice.dto.ProductAttributeDTO;
import com.nice.dto.VendorResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ProductAttributeMapper;
import com.nice.model.ProductAttribute;
import com.nice.model.ProductAttributeValue;
import com.nice.model.UserLogin;
import com.nice.repository.ProductAttributeRepository;
import com.nice.service.ProductAttributeService;
import com.nice.service.ProductAttributeValueService;
import com.nice.service.VendorService;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Service
@Transactional(rollbackFor = Throwable.class)
public class ProductAttributeServiceImpl implements ProductAttributeService {

	/**
	 *
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(ProductAttributeServiceImpl.class);

	private static final String NOT_FOUND = "product.attribute.not.found";

	@Autowired
	private ProductAttributeRepository productAttributeRepository;

	@Autowired
	private ProductAttributeMapper productAttributeMapper;

	@Autowired
	private ProductAttributeValueService productAttributeValueService;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private VendorService vendorService;

	@Override
	public ProductAttributeDTO addProductAttribute(final ProductAttributeDTO productAttributeDTO) throws NotFoundException, ValidationException {
		Long vendorId = getVendorIdForLoginUser();
		ProductAttribute productAttribute = productAttributeMapper.toEntity(productAttributeDTO);
		productAttribute.setVendorId(vendorId);
		productAttributeRepository.save(productAttribute);
		return productAttributeMapper.toDto(productAttribute);
	}

	@Override
	public ProductAttributeDTO updateProductAttribute(final ProductAttributeDTO productAttributeDTO) throws NotFoundException, ValidationException {
		Long vendorId = getVendorIdForLoginUser();
		if (productAttributeDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("product.attribute.id.not.null", null));
		}
		Optional<ProductAttribute> optExistingProductAttribute = productAttributeRepository.findById(productAttributeDTO.getId());
		if (!optExistingProductAttribute.isPresent()) {
			LOGGER.error("ProductAttribute is not exists for ProductAttributeId {} ", productAttributeDTO.getId());
			throw new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] { productAttributeDTO.getId() }));
		} else if (!optExistingProductAttribute.get().getVendorId().equals(vendorId)) {
			throw new NotFoundException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		productAttributeDTO.setVendorId(vendorId);
		return productAttributeMapper.toDto(productAttributeRepository.save(productAttributeMapper.toEntity(productAttributeDTO)));
	}

	@Override
	public ProductAttributeDTO getProductAttribute(final Long productAttributeId) throws NotFoundException, ValidationException {
		UserLogin userLogin = checkForUserLogin();
		ProductAttribute productAttribute = productAttributeRepository.findById(productAttributeId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] { productAttributeId })));
		/**
		 * Check if the given product attribute belongs to vendor
		 */
		if (userLogin.getEntityType() == null || (userLogin.getEntityId().equals(productAttribute.getVendorId()))) {
			ProductAttributeDTO productAttributeDto = productAttributeMapper.toDto(productAttribute);
			VendorResponseDTO vendorResponseDto = vendorService.getVendor(productAttributeDto.getVendorId());
			productAttributeDto.setVendorName(vendorResponseDto.getFirstName().concat(" ").concat(vendorResponseDto.getLastName()));
			return productAttributeDto;
		} else {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}

	}

	@Override
	public void changeStatus(final Long productAttributeId, final Boolean active) throws ValidationException, NotFoundException {
		ProductAttribute existingProductAttribute = productAttributeRepository.findById(productAttributeId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] { productAttributeId })));
		UserLogin userLogin = getUserLoginFromToken();
		/**
		 * Check for authorization
		 */
		if (!(userLogin.getEntityType() == null || existingProductAttribute.getVendorId().equals(userLogin.getEntityId()))) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		LOGGER.info("Existing  ProductAttribute details {} ", existingProductAttribute);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingProductAttribute.getActive().equals(active)) {
			if (active) {
				throw new ValidationException(messageByLocaleService.getMessage("product.attribute.active", null));
			} else {
				throw new ValidationException(messageByLocaleService.getMessage("product.attribute.deactive", null));
			}
		} else {
			if (Boolean.FALSE.equals(active)) {
				/**
				 * deActive all active product attribute values for this attribute
				 */
				List<ProductAttributeValue> productAttributeValueList = productAttributeValueService.getListByProductAttributeOrActive(productAttributeId,
						true);
				for (ProductAttributeValue productAttributeValue : productAttributeValueList) {
					productAttributeValueService.changeStatus(productAttributeValue.getId(), false);
				}
			}
			existingProductAttribute.setActive(active);
			productAttributeRepository.save(existingProductAttribute);
		}
	}

	@Override
	public Page<ProductAttribute> getList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords) throws ValidationException {
		UserLogin userLogin = checkForUserLogin();
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("name"));
		Long vendorId = userLogin.getEntityId();

		if (activeRecords != null) {
			return vendorId == null ? productAttributeRepository.findAllByActive(activeRecords, pageable)
					: productAttributeRepository.findAllByActiveAndVendorId(activeRecords, vendorId, pageable);
		} else {
			return vendorId == null ? productAttributeRepository.findAll(pageable) : productAttributeRepository.findAllByVendorId(vendorId, pageable);
		}
	}

	@Override
	public List<ProductAttribute> getAllActiveList() throws ValidationException {
		UserLogin userLogin = checkForUserLogin();
		Long vendorId = userLogin.getEntityId();
		return productAttributeRepository.findAllByActiveAndVendorId(true, vendorId);
	}

	@Override
	public boolean isExists(final ProductAttributeDTO productAttributeDTO) throws ValidationException {
		Long vendorId = getVendorIdForLoginUser();
		/**
		 * Only vendor can update the product attribute
		 */
		if (vendorId == null) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
		if (productAttributeDTO.getId() != null) {
			return productAttributeRepository.findByNameIgnoreCaseAndVendorIdAndIdNot(productAttributeDTO.getName(), vendorId, productAttributeDTO.getId())
					.isPresent();
		} else {
			return productAttributeRepository.findByNameIgnoreCaseAndVendorId(productAttributeDTO.getName(), vendorId).isPresent();
		}
	}

	@Override
	public ProductAttribute getProductAttributeDetail(final Long ProductAttributeId) throws NotFoundException {
		return productAttributeRepository.findById(ProductAttributeId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage(NOT_FOUND, new Object[] { ProductAttributeId })));
	}

	@Override
	public void deleteProductAttribute(final Long productAttributeId) {
		productAttributeRepository.deleteById(productAttributeId);
	}

	private Long getVendorIdForLoginUser() throws ValidationException {
		UserLogin userLogin = checkForUserLogin();
		if (!UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		} else {
			return userLogin.getEntityId();
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

}
