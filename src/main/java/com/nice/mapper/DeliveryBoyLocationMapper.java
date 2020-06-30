/**
 *
 */
package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.DeliveryBoyLocationDTO;
import com.nice.model.DeliveryBoyLocation;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 18, 2020
 */
@Component
public class DeliveryBoyLocationMapper {

	public DeliveryBoyLocationDTO toDto(final DeliveryBoyLocation deliveryBoyLocation) {
		DeliveryBoyLocationDTO deliveryBoyLocationDTO = new DeliveryBoyLocationDTO();
		BeanUtils.copyProperties(deliveryBoyLocation, deliveryBoyLocationDTO);
		deliveryBoyLocationDTO.setDeliveryBoyId(deliveryBoyLocation.getDeliveryBoy().getId());
		return deliveryBoyLocationDTO;
	}

	public DeliveryBoyLocation toEntity(final DeliveryBoyLocationDTO deliveryBoyLocationDTO) {
		DeliveryBoyLocation deliveryBoyLocation = new DeliveryBoyLocation();
		BeanUtils.copyProperties(deliveryBoyLocationDTO, deliveryBoyLocation);
		return deliveryBoyLocation;
	}

	public List<DeliveryBoyLocationDTO> toDtos(final List<DeliveryBoyLocation> deliveryBoyLocations) {
		List<DeliveryBoyLocationDTO> results = new ArrayList<>();
		for (DeliveryBoyLocation deliveryBoyLocation : deliveryBoyLocations) {
			results.add(toDto(deliveryBoyLocation));
		}
		return results;
	}
}
