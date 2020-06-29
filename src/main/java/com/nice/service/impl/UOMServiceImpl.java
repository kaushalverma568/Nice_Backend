package com.nice.service.impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.UOMDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.UOMMapper;
import com.nice.model.UOM;
import com.nice.repository.UOMRepository;
import com.nice.service.UOMService;

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

	@Override
	public void addUOM(final UOMDTO uomDTO) throws ValidationException, NotFoundException {
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

	@Override
	public void updateUOM(final UOMDTO resultUOMDTO) throws NotFoundException, ValidationException {
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
	public Page<UOM> getUOMList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords) throws NotFoundException {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("id"));
		if (activeRecords != null) {
			return uomRepository.findAllByActive(activeRecords, pageable);
		} else {
			return uomRepository.findAll(pageable);
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
				/**
				 * Currently commenting the product check, for generic project. But in actual implementation once product module is
				 * available it should be a part of code.
				 */
				// final ProductParamRequestDTO paramRequestDTO = new ProductParamRequestDTO();
				// paramRequestDTO.setUomId(uomId);
				// final List<ProductResponseDTO> productList = productService.getProductListBasedOnParams(paramRequestDTO, null, null,
				// true, null);
				// if (!productList.isEmpty()) {
				// throw new ValidationException(messageByLocaleService.getMessage("uom.can.not.deactive", null));
				// }
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

}
