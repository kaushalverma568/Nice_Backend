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
import com.nice.dto.VendorBankDetailsDTO;
import com.nice.dto.VendorDTO;
import com.nice.dto.VendorResponseDTO;
import com.nice.model.Vendor;
import com.nice.model.VendorBankDetails;
import com.nice.service.AssetService;
import com.nice.service.VendorCuisineService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 18, 2020
 */
@Component
public class VendorMapper {

	@Autowired
	private VendorCuisineService vendorCuisineService;

	@Autowired
	private AssetService assetService;

	public VendorResponseDTO toDto(final Vendor vendor) {
		VendorResponseDTO vendorResponseDTO = new VendorResponseDTO();
		BeanUtils.copyProperties(vendor, vendorResponseDTO);
		if (vendor.getSubscriptionPlan() != null) {
			vendorResponseDTO.setSubscriptionPlanId(vendor.getSubscriptionPlan().getId());
			vendorResponseDTO.setSubscriptionPlanName(vendor.getSubscriptionPlan().getName());
		}
		vendorResponseDTO.setCityId(vendor.getCity().getId());
		vendorResponseDTO.setCityName(vendor.getCity().getName());
		vendorResponseDTO.setCountryId(vendor.getCountry().getId());
		vendorResponseDTO.setCountryName(vendor.getCountry().getName());
		vendorResponseDTO.setPincodeId(vendor.getPincode().getId());
		vendorResponseDTO.setCodeValue(vendor.getPincode().getCodeValue());
		vendorResponseDTO.setBusinessCategoryId(vendor.getBusinessCategory().getId());
		vendorResponseDTO.setBusinessCategoryName(vendor.getBusinessCategory().getName());
		vendorResponseDTO.setVendorCuisines(vendorCuisineService.getVendorCuisineDetailListByVendor(vendor.getId(), true));
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendor.getProfilePictureName())) {
			vendorResponseDTO.setProfilePictureUrl(assetService.getGeneratedUrl(vendor.getProfilePictureName(), AssetConstant.VENDOR));
		}
		return vendorResponseDTO;
	}

	public VendorBankDetailsDTO toBankDetailsDTO(final VendorBankDetails vendorBankDetails) {
		VendorBankDetailsDTO vendorBankDetailsDTO = new VendorBankDetailsDTO();
		BeanUtils.copyProperties(vendorBankDetails, vendorBankDetailsDTO);
		vendorBankDetailsDTO.setVendorId(vendorBankDetails.getVendor().getId());
		return vendorBankDetailsDTO;
	}

	public Vendor toEntity(final VendorDTO vendorDTO) {
		Vendor vendor = new Vendor();
		BeanUtils.copyProperties(vendorDTO, vendor);
		return vendor;
	}

	public List<VendorResponseDTO> toDtos(final List<Vendor> vendors) {
		List<VendorResponseDTO> results = new ArrayList<>();
		for (Vendor vendor : vendors) {
			results.add(toDto(vendor));
		}
		return results;
	}
}
