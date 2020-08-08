/**
 *
 */
package com.nice.mapper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.nice.constant.AssetConstant;
import com.nice.constant.VendorStatus;
import com.nice.dto.VendorBankDetailsDTO;
import com.nice.dto.VendorBasicDetailDTO;
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
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendor.getStoreImageName())) {
			vendorResponseDTO.setStoreImageUrl(assetService.getGeneratedUrl(vendor.getStoreImageName(), AssetConstant.VENDOR));
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendor.getStoreDetailImageName())) {
			vendorResponseDTO.setStoreDetailImageUrl(assetService.getGeneratedUrl(vendor.getStoreDetailImageName(), AssetConstant.VENDOR));
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendor.getFeaturedImageName())) {
			vendorResponseDTO.setFeaturedImageUrl(assetService.getGeneratedUrl(vendor.getFeaturedImageName(), AssetConstant.VENDOR));
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendor.getStatus())) {
			final VendorStatus vendorOldStatus = VendorStatus.valueOf(VendorStatus.getByValue(vendorResponseDTO.getStatus()).name());
			if (vendorOldStatus.nextAdminStatus() == null) {
				vendorResponseDTO.setNextStatus(Collections.emptyList());
			} else {
				final List<VendorStatus> vendorStatusList = Arrays.asList(vendorOldStatus.nextAdminStatus());
				vendorResponseDTO.setNextStatus(vendorStatusList.stream().map(VendorStatus::getStatusValue).collect(Collectors.toList()));
			}
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
		vendor.setEmail(vendorDTO.getEmail().toLowerCase());
		return vendor;
	}

	public List<VendorResponseDTO> toDtos(final List<Vendor> vendors) {
		List<VendorResponseDTO> results = new ArrayList<>();
		for (Vendor vendor : vendors) {
			results.add(toDto(vendor));
		}
		return results;
	}

	public VendorBasicDetailDTO toBasicDto(final Vendor vendor) {
		VendorBasicDetailDTO vendorBasicDetailDTO = new VendorBasicDetailDTO();
		BeanUtils.copyProperties(vendor, vendorBasicDetailDTO);
		vendorBasicDetailDTO.setBusinessCategoryId(vendor.getBusinessCategory().getId());
		vendorBasicDetailDTO.setBusinessCategoryName(vendor.getBusinessCategory().getName());
		vendorBasicDetailDTO.setManageInventory(vendor.getBusinessCategory().getManageInventory());
		return vendorBasicDetailDTO;
	}
}
