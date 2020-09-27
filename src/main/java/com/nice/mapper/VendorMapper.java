/**
 *
 */
package com.nice.mapper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.stream.Collectors;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.constant.AssetConstant;
import com.nice.constant.VendorStatus;
import com.nice.dto.VendorAppResponseDTO;
import com.nice.dto.VendorBankDetailsDTO;
import com.nice.dto.VendorBasicDetailDTO;
import com.nice.dto.VendorDTO;
import com.nice.dto.VendorExport;
import com.nice.dto.VendorResponseDTO;
import com.nice.model.Vendor;
import com.nice.model.VendorBankDetails;
import com.nice.service.AssetService;
import com.nice.service.VendorCuisineService;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 18, 2020
 */
@Component
public class VendorMapper {

	@Autowired
	private VendorCuisineService vendorCuisineService;

	@Autowired
	private AssetService assetService;

	public VendorResponseDTO toDto(final Vendor vendor, final boolean isDetailResponse) {
		Locale locale = LocaleContextHolder.getLocale();
		VendorResponseDTO vendorResponseDTO = new VendorResponseDTO();
		BeanUtils.copyProperties(vendor, vendorResponseDTO);
		if (locale.getLanguage().equals("en")) {
			vendorResponseDTO.setFirstName(vendor.getFirstNameEnglish());
			vendorResponseDTO.setLastName(vendor.getLastNameEnglish());
			vendorResponseDTO.setBlock(vendor.getBlockEnglish());
			vendorResponseDTO.setBuilding(vendor.getBuildingEnglish());
			vendorResponseDTO.setStreet(vendor.getStreetEnglish());
			vendorResponseDTO.setArea(vendor.getAreaEnglish());
			vendorResponseDTO.setBusinessCategoryName(vendor.getBusinessCategory().getNameEnglish());
			vendorResponseDTO.setStoreName(vendor.getStoreNameEnglish());
		} else {
			vendorResponseDTO.setFirstName(vendor.getFirstNameArabic());
			vendorResponseDTO.setLastName(vendor.getLastNameArabic());
			vendorResponseDTO.setBlock(vendor.getBlockArabic());
			vendorResponseDTO.setBuilding(vendor.getBuildingArabic());
			vendorResponseDTO.setStreet(vendor.getStreetArabic());
			vendorResponseDTO.setArea(vendor.getAreaArabic());
			vendorResponseDTO.setBusinessCategoryName(vendor.getBusinessCategory().getNameArabic());
			vendorResponseDTO.setStoreName(vendor.getStoreNameArabic());
		}
		if (vendor.getSubscriptionPlan() != null) {
			vendorResponseDTO.setSubscriptionPlanId(vendor.getSubscriptionPlan().getId());
			vendorResponseDTO.setSubscriptionPlanName(
					locale.getLanguage().equals("en") ? vendor.getSubscriptionPlan().getNameEnglish() : vendor.getSubscriptionPlan().getNameArabic());
		}
		vendorResponseDTO.setBusinessCategoryId(vendor.getBusinessCategory().getId());
		vendorResponseDTO.setManageInventory(vendor.getBusinessCategory().getManageInventory());
		vendorResponseDTO.setPincodeId(vendor.getPincode().getId());
		vendorResponseDTO.setCodeValue(vendor.getPincode().getCodeValue());
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendor.getStoreImageName())) {
			vendorResponseDTO.setStoreImageUrl(assetService.getGeneratedUrl(vendor.getStoreImageName(), AssetConstant.VENDOR));
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendor.getFeaturedImageName())) {
			vendorResponseDTO.setFeaturedImageUrl(assetService.getGeneratedUrl(vendor.getFeaturedImageName(), AssetConstant.VENDOR));
		}
		if (vendor.getRating() == null) {
			vendorResponseDTO.setRating(0D);
		}
		if (vendor.getNoOfRating() == null) {
			vendorResponseDTO.setNoOfRating(0L);
		}
		if (isDetailResponse) {
			vendorResponseDTO.setCityId(vendor.getCity().getId());
			vendorResponseDTO.setVendorCuisines(vendorCuisineService.getVendorCuisineDetailListByVendor(vendor.getId(), true));
			vendorResponseDTO.setCityName(locale.getLanguage().equals("en") ? vendor.getCity().getNameEnglish() : vendor.getCity().getNameArabic());
			if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendor.getStoreDetailImageName())) {
				vendorResponseDTO.setStoreDetailImageUrl(assetService.getGeneratedUrl(vendor.getStoreDetailImageName(), AssetConstant.VENDOR));
			}
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
		Locale locale = LocaleContextHolder.getLocale();
		VendorBankDetailsDTO vendorBankDetailsDTO = new VendorBankDetailsDTO();
		BeanUtils.copyProperties(vendorBankDetails, vendorBankDetailsDTO);
		if (locale.getLanguage().equals("en")) {
			vendorBankDetailsDTO.setBankName(vendorBankDetails.getBankNameEnglish());
			vendorBankDetailsDTO.setBranchName(vendorBankDetails.getBranchNameEnglish());
			vendorBankDetailsDTO.setAccountName(vendorBankDetails.getAccountNameEnglish());
			vendorBankDetailsDTO.setBranchCity(vendorBankDetails.getBranchCityEnglish());
		} else {
			vendorBankDetailsDTO.setBankName(vendorBankDetails.getBankNameArabic());
			vendorBankDetailsDTO.setBranchName(vendorBankDetails.getBranchNameArabic());
			vendorBankDetailsDTO.setAccountName(vendorBankDetails.getAccountNameArabic());
			vendorBankDetailsDTO.setBranchCity(vendorBankDetails.getBranchCityArabic());
		}
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
			results.add(toDto(vendor, false));
		}
		return results;
	}

	public VendorBasicDetailDTO toBasicDto(final Vendor vendor) {
		Locale locale = LocaleContextHolder.getLocale();
		VendorBasicDetailDTO vendorBasicDetailDTO = new VendorBasicDetailDTO();
		BeanUtils.copyProperties(vendor, vendorBasicDetailDTO);
		vendorBasicDetailDTO.setBusinessCategoryId(vendor.getBusinessCategory().getId());
		vendorBasicDetailDTO.setManageInventory(vendor.getBusinessCategory().getManageInventory());
		if (locale.getLanguage().equals("en")) {
			vendorBasicDetailDTO.setBusinessCategoryName(vendor.getBusinessCategory().getNameEnglish());
		} else {
			vendorBasicDetailDTO.setBusinessCategoryName(vendor.getBusinessCategory().getNameArabic());
		}
		return vendorBasicDetailDTO;
	}

	public VendorAppResponseDTO toAppDto(final Vendor vendor, final boolean isDetailResponse) {
		VendorAppResponseDTO vendorAppResponseDTO = new VendorAppResponseDTO();
		BeanUtils.copyProperties(vendor, vendorAppResponseDTO);
		if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
			vendorAppResponseDTO.setStoreName(vendor.getStoreNameEnglish());
		} else {
			vendorAppResponseDTO.setStoreName(vendor.getStoreNameArabic());
		}
		if (vendor.getRating() == null) {
			vendorAppResponseDTO.setRating(0D);
		}
		if (vendor.getNoOfRating() == null) {
			vendorAppResponseDTO.setNoOfRating(0L);
		}
		if (isDetailResponse) {
			vendorAppResponseDTO.setVendorCuisines(vendorCuisineService.getVendorCuisineDetailListByVendor(vendor.getId(), true));
			if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendor.getStoreDetailImageName())) {
				vendorAppResponseDTO.setStoreDetailImageUrl(assetService.getGeneratedUrl(vendor.getStoreDetailImageName(), AssetConstant.VENDOR));
			}
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendor.getStoreImageName())) {
			vendorAppResponseDTO.setStoreImageUrl(assetService.getGeneratedUrl(vendor.getStoreImageName(), AssetConstant.VENDOR));
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendor.getFeaturedImageName())) {
			vendorAppResponseDTO.setFeaturedImageUrl(assetService.getGeneratedUrl(vendor.getFeaturedImageName(), AssetConstant.VENDOR));
		}
		return vendorAppResponseDTO;
	}

	public VendorExport toExportDTO(final Vendor vendor) {
		Locale locale = LocaleContextHolder.getLocale();
		VendorExport vendorExport = new VendorExport();
		BeanUtils.copyProperties(vendor, vendorExport);
		if (locale.getLanguage().equals("en")) {
			vendorExport.setFirstName(vendor.getFirstNameEnglish());
			vendorExport.setLastName(vendor.getLastNameEnglish());
			vendorExport.setStoreName(vendor.getStoreNameEnglish());
		} else {
			vendorExport.setFirstName(vendor.getFirstNameArabic());
			vendorExport.setLastName(vendor.getLastNameArabic());
			vendorExport.setStoreName(vendor.getStoreNameArabic());
		}
		return vendorExport;
	}
}
