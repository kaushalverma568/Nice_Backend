package com.nice.service.impl;

import java.util.List;

import javax.transaction.Transactional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.Constant;
import com.nice.constant.UserType;
import com.nice.dto.AddonsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.AddonsMapper;
import com.nice.model.Addons;
import com.nice.model.ProductAddons;
import com.nice.model.UserLogin;
import com.nice.model.Vendor;
import com.nice.repository.AddonsRepository;
import com.nice.service.AddonsService;
import com.nice.service.ProductAddonsService;
import com.nice.service.VendorService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 14-Jul-2020
 */
@Service("addonsService")
@Transactional(rollbackOn = Throwable.class)
public class AddonsServiceImpl implements AddonsService {

	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(AddonsServiceImpl.class);

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private AddonsRepository addonsRepository;

	@Autowired
	private AddonsMapper addonsMapper;

	@Autowired
	private VendorService vendorService;

	@Autowired
	private ProductAddonsService productAddonsService;

	@Override
	public Boolean isExists(final AddonsDTO addonsDto) throws ValidationException, NotFoundException {
		Long vendorId = getVendorIdForLoginUser();
		Vendor vendor = vendorService.getVendorDetail(vendorId);
		if (addonsDto.getId() != null) {
			return addonsRepository.findByNameIgnoreCaseAndVendorAndIdNot(addonsDto.getName(), vendor, addonsDto.getId()).isPresent();
		} else {
			return addonsRepository.findByNameIgnoreCaseAndVendor(addonsDto.getName(), vendor).isPresent();
		}
	}

	@Override
	public void addAddons(final AddonsDTO addonsDTO) throws NotFoundException, ValidationException {
		Long vendorId = getVendorIdForLoginUser();
		Addons addons = addonsMapper.toEntity(addonsDTO);
		addons.setVendor(vendorService.getVendorDetail(vendorId));
		addonsRepository.save(addons);
	}

	@Override
	public void updateAddons(final AddonsDTO addonsDTO) throws NotFoundException, ValidationException {
		Long vendorId = getVendorIdForLoginUser();
		if (addonsDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("addons.id.not.null", null));
		} else {
			Addons existingAddons = getAddonsById(addonsDTO.getId());
			/**
			 * if vendor is different than previous then throw an error
			 */
			if (!existingAddons.getVendor().getId().equals(vendorId)) {
				throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
			}
			final Addons addons = addonsMapper.toEntity(addonsDTO);
			addons.setVendor(existingAddons.getVendor());
			addonsRepository.save(addons);
		}
	}

	@Override
	public AddonsDTO getAddonsDetailById(final Long id) throws NotFoundException {
		return addonsMapper.toDto(getAddonsById(id));
	}

	@Override
	public Addons getAddonsById(final Long id) throws NotFoundException {
		return addonsRepository.findById(id)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("addons.not.found", new Object[] { id })));
	}

	@Override
	public Page<Addons> getAddonsList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords, final String searchKeyword,
			final Long vendorId) throws NotFoundException {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("name"));
		if (vendorId != null) {
			Vendor vendor = vendorService.getVendorDetail(vendorId);
			if (activeRecords != null) {
				if (searchKeyword != null) {
					return addonsRepository.findAllByActiveAndNameContainingIgnoreCaseAndVendor(activeRecords, searchKeyword, vendor, pageable);
				} else {
					return addonsRepository.findAllByActiveAndVendor(activeRecords, vendor, pageable);
				}
			} else {
				if (searchKeyword != null) {
					return addonsRepository.findAllByNameContainingIgnoreCaseAndVendor(searchKeyword, vendor, pageable);
				} else {
					return addonsRepository.findAllByVendor(vendor, pageable);
				}
			}
		} else {
			return findAllByActiveAndSearchKeyword(activeRecords, searchKeyword, pageable);
		}
	}

	/**
	 * @param  activeRecords
	 * @param  searchKeyword
	 * @param  pageable
	 * @return
	 */
	private Page<Addons> findAllByActiveAndSearchKeyword(final Boolean activeRecords, final String searchKeyword, final Pageable pageable) {
		if (activeRecords != null) {
			if (searchKeyword != null) {
				return addonsRepository.findAllByActiveAndNameContainingIgnoreCase(activeRecords, searchKeyword, pageable);
			} else {
				return addonsRepository.findAllByActive(activeRecords, pageable);
			}

		} else {
			if (searchKeyword != null) {
				return addonsRepository.findAllByNameContainingIgnoreCase(searchKeyword, pageable);
			} else {
				return addonsRepository.findAll(pageable);
			}
		}
	}

	@Override
	public void changeStatus(final Long addonsId, final Boolean active) throws NotFoundException, ValidationException {
		final Addons existingAddons = getAddonsById(addonsId);
		LOGGER.info("Existing category details {} ", existingAddons);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingAddons.getActive().equals(active)) {
			throw new ValidationException(
					messageByLocaleService.getMessage(Boolean.TRUE.equals(active) ? "addons.already.active" : "addons.already.deactive", null));
		} else {
			/**
			 * deactivate all product addons based on this addons while deactivate addon
			 */
			if (Boolean.FALSE.equals(active)) {
				LOGGER.info("DeActivating  Addons {}", existingAddons);
				final List<ProductAddons> productAddonsList = productAddonsService.getListByAddonsId(addonsId);
				for (final ProductAddons productAddons : productAddonsList) {
					productAddonsService.changeStatus(productAddons.getId(), Boolean.FALSE);
				}
			} else {
				LOGGER.info("Activating  Addons");
			}
			existingAddons.setActive(active);
			addonsRepository.save(existingAddons);
		}
	}

	private Long getVendorIdForLoginUser() throws ValidationException {
		UserLogin userLogin = checkForUserLogin();
		if (!UserType.VENDOR.name().equals(userLogin.getEntityType())) {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		} else {
			return userLogin.getEntityId();
		}
	}

	private UserLogin checkForUserLogin() throws ValidationException {
		UserLogin userLogin = getUserLoginFromToken();
		if (userLogin == null) {
			throw new ValidationException(messageByLocaleService.getMessage("login.first", null));
		} else {
			return userLogin;
		}
	}

	private UserLogin getUserLoginFromToken() {
		Object principal = SecurityContextHolder.getContext().getAuthentication().getPrincipal();
		if (Constant.ANONYMOUS_USER.equals(principal)) {
			return null;
		}
		return ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
	}
}
