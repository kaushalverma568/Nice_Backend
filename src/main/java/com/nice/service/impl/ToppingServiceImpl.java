package com.nice.service.impl;

import java.util.List;

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
import com.nice.dto.ToppingDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.ToppingMapper;
import com.nice.model.ProductTopping;
import com.nice.model.Topping;
import com.nice.model.UserLogin;
import com.nice.repository.ToppingRepository;
import com.nice.service.ProductToppingService;
import com.nice.service.ToppingService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 26-06-2020
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
	public Page<Topping> getToppingList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final String searchKeyword) {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("name"));
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		Long vendorId = userLogin.getEntityId();
		if (activeRecords != null) {
			if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(searchKeyword)) {
				return toppingRepository.findAllByActiveAndNameContainingIgnoreCaseAndVendorId(activeRecords, searchKeyword, vendorId, pageable);
			} else {
				return toppingRepository.findAllByActiveAndVendorId(activeRecords, vendorId, pageable);
			}
		} else {
			if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(searchKeyword)) {
				return toppingRepository.findAllByNameContainingIgnoreCaseAndVendorId(searchKeyword, vendorId, pageable);
			} else {
				return toppingRepository.findAllByVendorId(vendorId, pageable);
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
}
