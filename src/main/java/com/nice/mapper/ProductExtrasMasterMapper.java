package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.ProductExtrasMasterDTO;
import com.nice.model.ProductExtrasMaster;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 02-Jul-2020
 */
@Component
public class ProductExtrasMasterMapper {

	public ProductExtrasMasterDTO toDto(final ProductExtrasMaster productExtrasMaster) {
		ProductExtrasMasterDTO productExtrasMasterResponseDTO = new ProductExtrasMasterDTO();
		BeanUtils.copyProperties(productExtrasMaster, productExtrasMasterResponseDTO);
		return productExtrasMasterResponseDTO;
	}

	public ProductExtrasMaster toEntity(final ProductExtrasMasterDTO productExtrasMasterDTO) {
		ProductExtrasMaster productExtrasMaster = new ProductExtrasMaster();
		BeanUtils.copyProperties(productExtrasMasterDTO, productExtrasMaster);
		return productExtrasMaster;
	}

	public List<ProductExtrasMasterDTO> toDtos(final List<ProductExtrasMaster> productExtrasMasterList) {
		List<ProductExtrasMasterDTO> results = new ArrayList<>();
		for (ProductExtrasMaster ProductExtrasMaster : productExtrasMasterList) {
			results.add(toDto(ProductExtrasMaster));
		}
		return results;
	}
}
