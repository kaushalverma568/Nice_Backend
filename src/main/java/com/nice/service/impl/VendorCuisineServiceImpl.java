package com.nice.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.VendorCuisineDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.VendorCuisineMapper;
import com.nice.model.Cuisine;
import com.nice.model.Product;
import com.nice.model.Vendor;
import com.nice.model.VendorCuisine;
import com.nice.repository.ProductRepository;
import com.nice.repository.VendorCuisineRepository;
import com.nice.service.CuisineService;
import com.nice.service.ProductService;
import com.nice.service.VendorCuisineService;
import com.nice.service.VendorService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 01-Jul-2020
 */
@Transactional(rollbackFor = Throwable.class)
@Service("vendorCuisineMapService")
public class VendorCuisineServiceImpl implements VendorCuisineService {

	@Autowired
	private VendorCuisineRepository vendorCuisineRepository;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private CuisineService cuisineService;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private VendorCuisineMapper vendorCuisineMapper;

	@Autowired
	private ProductRepository productRepository;

	@Autowired
	private ProductService productService;

	@Override
	public void addUpdateVendorCuisine(final VendorCuisineDTO vendorCuisineMapDTO) throws ValidationException, NotFoundException {
		if (isAlreadyExist(vendorCuisineMapDTO)) {
			throw new ValidationException(messageByLocaleService.getMessage("vendor.cuisine.already.exist",
					new Object[] { vendorCuisineMapDTO.getCuisineId(), vendorCuisineMapDTO.getVendorId() }));
		}
		VendorCuisine vendorCuisineMap = vendorCuisineMapper.toEntity(vendorCuisineMapDTO);
		vendorCuisineMap.setVendor(vendorService.getVendorDetail(vendorCuisineMapDTO.getVendorId()));
		vendorCuisineMap.setCuisine(cuisineService.getCuisineDetails(vendorCuisineMapDTO.getCuisineId()));
		vendorCuisineRepository.save(vendorCuisineMap);
	}

	@Override
	public boolean isAlreadyExist(final VendorCuisineDTO vendorCuisineMapDTO) {
		if (vendorCuisineMapDTO.getId() != null) {
			return vendorCuisineRepository
					.findByVendorIdAndCuisineIdAndIdNot(vendorCuisineMapDTO.getVendorId(), vendorCuisineMapDTO.getCuisineId(), vendorCuisineMapDTO.getId())
					.isPresent();
		} else {
			return vendorCuisineRepository.findByVendorIdAndCuisineId(vendorCuisineMapDTO.getVendorId(), vendorCuisineMapDTO.getCuisineId()).isPresent();
		}
	}

	@Override
	public List<VendorCuisine> getVendorCuisineListByVendor(final Long vendorId, final Boolean active) {
		if (active == null) {
			return vendorCuisineRepository.findAllByVendorId(vendorId);
		} else {
			return vendorCuisineRepository.findAllByVendorIdAndActive(vendorId, active);
		}
	}

	@Override
	public VendorCuisine getVendorCuisineById(final Long id) throws NotFoundException {
		return vendorCuisineRepository.findById(id)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("vendor.cuisine.not.found", new Object[] { id })));
	}

	@Override
	public void changeStatus(final Long id, final Boolean active) throws ValidationException, NotFoundException {
		VendorCuisine vendorCuisine = getVendorCuisineById(id);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (active.equals(vendorCuisine.getActive())) {
			throw new ValidationException(messageByLocaleService.getMessage("vendor.cuisine.active.deactive",
					new Object[] { (Boolean.TRUE.equals(active) ? "active" : "deActive") }));
		} else {
			changeStatusForDependentEntity(active, vendorCuisine);
			vendorCuisine.setActive(active);
			vendorCuisineRepository.save(vendorCuisine);
		}
	}

	private void changeStatusForDependentEntity(final Boolean active, final VendorCuisine vendorCuisineMap) throws ValidationException, NotFoundException {
		if (Boolean.FALSE.equals(active)) {
			/**
			 * if active is false then all product with vendor and cuisine will be deactive
			 */
			List<Product> productList = productRepository.findAllByVendorIdAndCuisineId(vendorCuisineMap.getVendor().getId(),
					vendorCuisineMap.getCuisine().getId());
			for (Product product : productList) {
				productService.changeStatus(product.getId(), active);
			}
		} else {
			if (Boolean.FALSE.equals(vendorCuisineMap.getVendor().getActive())) {
				throw new ValidationException(messageByLocaleService.getMessage("activate.vendor.first", null));
			}
			if (Boolean.FALSE.equals(vendorCuisineMap.getCuisine().getActive())) {
				throw new ValidationException(messageByLocaleService.getMessage("activate.cuisine.first", null));
			}
		}
	}

	@Override
	public void bulkChangeAllStatus(final List<VendorCuisine> vendorCuisines, final Boolean active) throws ValidationException, NotFoundException {
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		}
		for (VendorCuisine vendorCuisine : vendorCuisines) {
			changeStatus(vendorCuisine.getId(), active);
		}

	}

	@Override
	public List<VendorCuisineDTO> getVendorCuisineDetailListByVendor(final Long vendorId, final Boolean active) {
		return vendorCuisineMapper.toDtos(getVendorCuisineListByVendor(vendorId, active));
	}

	@Override
	public VendorCuisine getVendorCuisineByVendorIdAndCuisineId(final Long vendorId, final Long cuisineId) throws NotFoundException {
		Vendor vendor = vendorService.getVendorDetail(vendorId);
		Cuisine cuisine = cuisineService.getCuisineDetails(cuisineId);
		if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
			return vendorCuisineRepository.findAllByVendorIdAndCuisineId(vendorId, cuisineId)
					.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("vendor.cuisine.not.found",
							new Object[] { cuisine.getNameEnglish(), vendor.getFirstNameEnglish().concat(vendor.getLastNameEnglish()) })));
		} else {
			return vendorCuisineRepository.findAllByVendorIdAndCuisineId(vendorId, cuisineId)
					.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("vendor.cuisine.not.found",
							new Object[] { cuisine.getNameArabic(), vendor.getFirstNameArabic().concat(vendor.getLastNameArabic()) })));
		}
	}

	@Override
	public List<VendorCuisine> getVendorCuisineListByCuisine(final Long cuisineId, final Boolean active) throws NotFoundException {
		if (cuisineId != null) {
			Cuisine cuisine = cuisineService.getCuisineDetails(cuisineId);
			if (active != null) {
				return vendorCuisineRepository.findAllByCuisineAndActive(cuisine, active);
			} else {
				return vendorCuisineRepository.findAllByCuisine(cuisine);
			}
		} else {
			if (active != null) {
				return vendorCuisineRepository.findAllByActive(active);
			} else {
				return vendorCuisineRepository.findAll();
			}
		}
	}

}
