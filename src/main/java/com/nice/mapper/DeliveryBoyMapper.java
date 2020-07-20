/**
 *
 */
package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.nice.constant.AssetConstant;
import com.nice.dto.DeliveryBoyDTO;
import com.nice.dto.DeliveryBoyResponseDTO;
import com.nice.model.DeliveryBoy;
import com.nice.service.AssetService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 18, 2020
 */
@Component
public class DeliveryBoyMapper {

	@Autowired
	private AssetService assetService;

	public DeliveryBoyResponseDTO toDto(final DeliveryBoy deliveryBoy) {
		DeliveryBoyResponseDTO deliveryBoyResponseDTO = new DeliveryBoyResponseDTO();
		BeanUtils.copyProperties(deliveryBoy, deliveryBoyResponseDTO);
		deliveryBoyResponseDTO.setRegisteredOn(deliveryBoy.getCreatedAt());
		deliveryBoyResponseDTO.setName(deliveryBoy.getFirstName() + " " + deliveryBoy.getLastName());
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(deliveryBoy.getProfilePictureName())) {
			deliveryBoyResponseDTO.setProfilePictureUrl(assetService.getGeneratedUrl(deliveryBoy.getProfilePictureName(), AssetConstant.DELIVERY_BOY));
		}
		return deliveryBoyResponseDTO;
	}

	public DeliveryBoy toEntity(final DeliveryBoyDTO deliveryBoyDTO) {
		DeliveryBoy deliveryBoy = new DeliveryBoy();
		BeanUtils.copyProperties(deliveryBoyDTO, deliveryBoy);
		deliveryBoy.setEmail(deliveryBoyDTO.getEmail().toLowerCase());
		return deliveryBoy;
	}

	public List<DeliveryBoyResponseDTO> toDtos(final List<DeliveryBoy> deliveryBoys) {
		List<DeliveryBoyResponseDTO> results = new ArrayList<>();
		for (DeliveryBoy deliveryBoy : deliveryBoys) {
			results.add(toDto(deliveryBoy));
		}
		return results;
	}
}
