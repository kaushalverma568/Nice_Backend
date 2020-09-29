package com.nice.service.impl;

import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
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
	public DeliveryBoyLocationDTO getDeliveryBoyLocation(final Long deliveryBoyLocationId) throws NotFoundException {
		return deliveryBoyLocationMapper.toDto(getDeliveryBoyLocationDetails(deliveryBoyLocationId));
	}

	@Override
	public DeliveryBoyLocation getDeliveryBoyLocationByDeliveryBoyId(final Long deliveryBoyId) throws NotFoundException {
		return deliveryBoyLocationRepository.findByDeliveryBoy(deliveryBoyService.getDeliveryBoyDetail(deliveryBoyId))
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("deliveryboy.location.not.found", null)));
	}

	@Override
	public DeliveryBoyLocation getDeliveryBoyLocationDetails(final Long deliveryBoyLocationId) throws NotFoundException {
		return deliveryBoyLocationRepository.findById(deliveryBoyLocationId).orElseThrow(
				() -> new NotFoundException(messageByLocaleService.getMessage("deliveryboy.location.not.found", new Object[] { deliveryBoyLocationId })));
	}

}
