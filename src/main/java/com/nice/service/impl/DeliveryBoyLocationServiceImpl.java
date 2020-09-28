package com.nice.service.impl;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.dto.DeliveryBoyLocationDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.DeliveryBoyLocationMapper;
import com.nice.model.DeliveryBoy;
import com.nice.model.DeliveryBoyLocation;
import com.nice.repository.DeliveryBoyLocationRepository;
import com.nice.service.DeliveryBoyLocationService;
import com.nice.service.DeliveryBoyService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 19, 2020
 */
@Service("deliveryBoyLocationService")
@Transactional(rollbackFor = Throwable.class)
public class DeliveryBoyLocationServiceImpl implements DeliveryBoyLocationService {

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private DeliveryBoyLocationRepository deliveryBoyLocationRepository;

	@Autowired
	private DeliveryBoyLocationMapper deliveryBoyLocationMapper;

	@Autowired
	private DeliveryBoyService deliveryBoyService;

	@Override
	public void addUpdateDeliveryBoyLocation(final DeliveryBoyLocationDTO deliveryBoyLocationDTO) throws ValidationException, NotFoundException {
		DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(deliveryBoyLocationDTO.getDeliveryBoyId());
		DeliveryBoyLocation deliveryBoyLocation = deliveryBoyLocationMapper.toEntity(deliveryBoyLocationDTO);
		Optional<DeliveryBoyLocation> optDeliveryBoyLocation = deliveryBoyLocationRepository.findByDeliveryBoy(deliveryBoy);
		if (optDeliveryBoyLocation.isPresent()) {
			deliveryBoyLocation.setId(optDeliveryBoyLocation.get().getId());
		}
		deliveryBoyLocation.setDeliveryBoy(deliveryBoy);
		deliveryBoyLocation.setActive(true);
		deliveryBoyLocationRepository.save(deliveryBoyLocation);
	}

	@Override
	public void updateDeliveryBoyLocation(final DeliveryBoyLocationDTO deliveryBoyLocationDTO) throws NotFoundException, ValidationException {
		if (deliveryBoyLocationDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("deliveryboy.location.id.not.null", null));
		}
		DeliveryBoyLocation existingDeliveryBoyLocation = getDeliveryBoyLocationDetails(deliveryBoyLocationDTO.getId());
		if (!existingDeliveryBoyLocation.getDeliveryBoy().getId().equals(deliveryBoyLocationDTO.getDeliveryBoyId())) {
			throw new ValidationException(messageByLocaleService.getMessage("deliveryboy.id.not.unique", null));
		}
		DeliveryBoyLocation deliveryBoyLocation = deliveryBoyLocationMapper.toEntity(deliveryBoyLocationDTO);
		deliveryBoyLocation.setDeliveryBoy(deliveryBoyService.getDeliveryBoyDetail(deliveryBoyLocationDTO.getDeliveryBoyId()));
		deliveryBoyLocation.setActive(true);
		deliveryBoyLocationRepository.save(deliveryBoyLocation);
	}

	@Override
	public DeliveryBoyLocationDTO getDeliveryBoyLocation(final Long deliveryBoyLocationId) throws NotFoundException {
		return deliveryBoyLocationMapper.toDto(getDeliveryBoyLocationDetails(deliveryBoyLocationId));
	}

	@Override
	public DeliveryBoyLocation getDeliveryBoyLatestLocation(final Long deliveryBoyId) throws NotFoundException {
		return deliveryBoyLocationRepository.findFirstByDeliveryBoyOrderByUpdatedAtDesc(deliveryBoyService.getDeliveryBoyDetail(deliveryBoyId))
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("deliveryboy.location.not.found", null)));
	}

	@Override
	public DeliveryBoyLocation getDeliveryBoyLocationDetails(final Long deliveryBoyLocationId) throws NotFoundException {
		return deliveryBoyLocationRepository.findById(deliveryBoyLocationId).orElseThrow(
				() -> new NotFoundException(messageByLocaleService.getMessage("deliveryboy.location.not.found", new Object[] { deliveryBoyLocationId })));
	}

	@Override
	public Page<DeliveryBoyLocation> getDeliveryBoyLocationList(final Integer pageNumber, final Integer pageSize, final Long deliveryBoyId)
			throws NotFoundException {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("id"));
		if (deliveryBoyId != null) {
			DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(deliveryBoyId);
			return deliveryBoyLocationRepository.findAllByDeliveryBoy(deliveryBoy, pageable);
		} else {
			return deliveryBoyLocationRepository.findAll(pageable);
		}
	}

	@Override
	public List<DeliveryBoyLocation> getDeliveryBoyLocationList(final Long deliveryBoyId, final Boolean isLatestLocationRequired)
			throws NotFoundException, ValidationException {
		if (deliveryBoyId == null) {
			throw new ValidationException(messageByLocaleService.getMessage("deliveryboy.id.not.null", null));
		} else {
			DeliveryBoy deliveryBoy = deliveryBoyService.getDeliveryBoyDetail(deliveryBoyId);
			if (isLatestLocationRequired.booleanValue()) {
				return deliveryBoyLocationRepository.findAllByDeliveryBoy(deliveryBoy);
			} else {
				List<DeliveryBoyLocation> locationList = deliveryBoyLocationRepository.findAllByDeliveryBoyOrderByUpdatedAtDesc(deliveryBoy);
				if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(locationList)) {
					locationList.remove(0);
				}
				return locationList;
			}
		}
	}
}
