package com.nice.service;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.VendorBankDetailsDTO;
import com.nice.dto.VendorDTO;
import com.nice.dto.VendorFilterDTO;
import com.nice.dto.VendorResponseDTO;
import com.nice.dto.VendorRestaurantDetailsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Vendor;
import com.nice.model.VendorBankDetails;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 25, 2020
 */

public interface VendorService {
	/**
	 * persist vendor object
	 *
	 * @param vendorDTO
	 * @param profilePicture
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addVendor(VendorDTO vendorDTO, MultipartFile profilePicture) throws ValidationException, NotFoundException;

	/**
	 * get DTO object of vendor
	 *
	 * @param vendorId
	 * @return
	 * @throws NotFoundException
	 */
	VendorResponseDTO getVendor(final Long vendorId) throws NotFoundException;

	/**
	 * change status of vendor (active/deActive)
	 *
	 * @param vendorId
	 * @param isActive
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	String changeStatus(Long vendorId, Boolean isActive) throws NotFoundException, ValidationException;

	/**
	 * check vendor duplication and returning Boolean value.
	 *
	 * @param vendorDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Boolean isVendorExists(VendorDTO vendorDTO);

	/**
	 * get detail object of vendor
	 *
	 * @param vendorId
	 * @return
	 * @throws NotFoundException
	 */
	Vendor getVendorDetail(Long vendorId) throws NotFoundException;

	/**
	 * check is email exist in user login at the time of create or update delivery
	 * boy
	 *
	 * @param vendorDTO
	 * @return
	 */
	Boolean isUserLoginExists(VendorDTO vendorDTO);

	/**
	 * upload profile picture
	 *
	 * @param profilePicture
	 * @param vendorId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void uploadProfilePicture(MultipartFile profilePicture, Long vendorId) throws NotFoundException, ValidationException;

	/**
	 * Update bank details
	 *
	 * @param vendorBankDetailsDTO
	 * @throws NotFoundException
	 */
	void updateBankDetails(VendorBankDetailsDTO vendorBankDetailsDTO) throws NotFoundException;

	/**
	 * get page of delivery boy by parameters
	 *
	 * @param pageNumber
	 * @param pageSize
	 * @param activeRecords
	 * @param isEmailVerified
	 * @return
	 * @throws NotFoundException
	 */
	Page<Vendor> getVendorList(Integer pageNumber, Integer pageSize, Boolean activeRecords, Boolean isEmailVerified) throws NotFoundException;

	/**
	 * update email verified status of delivery boy
	 *
	 * @param vendorId
	 * @throws NotFoundException
	 */
	void verifyEmail(Long vendorId) throws NotFoundException;

	/**
	 * remove profile picture
	 *
	 * @param vendorId
	 * @throws NotFoundException
	 */
	void deleteProfilePicture(Long vendorId) throws NotFoundException;

	/**
	 * update vendor's personal details
	 *
	 * @param vendorDTO
	 * @param userId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	String updatePersonalDetails(VendorDTO vendorDTO, MultipartFile profilePicture) throws NotFoundException, ValidationException;

	/**
	 * add update subscription plan
	 *
	 * @param vendorId
	 * @param subscriptionPlanId
	 * @param userId
	 * @throws NotFoundException
	 */
	void addUpdateSubscriptionPlan(Long vendorId, Long subscriptionPlanId) throws NotFoundException;

	/**
	 * update restaurant details
	 *
	 * @param vendorRestaurantDetailsDTO
	 * @param userId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateRestaurantDetails(VendorRestaurantDetailsDTO vendorRestaurantDetailsDTO) throws NotFoundException, ValidationException;

	/**
	 * update order service is unable or not for the vendor
	 *
	 * @param vendorId
	 * @param isOrderServiceEnable
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void updateOrderServiceEnableForVendor(Long vendorId, Boolean isOrderServiceEnable) throws ValidationException, NotFoundException;

	/**
	 * get vendor count based on filter
	 *
	 * @param vendorFilterDTO
	 * @return
	 */
	Long getVendorCountBasedOnParams(VendorFilterDTO vendorFilterDTO);

	/**
	 * get vendor list based on parameters
	 *
	 * @param startIndex
	 * @param pageSize
	 * @param vendorFilterDTO
	 * @return
	 * @throws ValidationException
	 */
	List<Vendor> getVendorListBasedOnParams(Integer startIndex, Integer pageSize, VendorFilterDTO vendorFilterDTO) throws ValidationException;

	/**
	 * get vendor bank detail
	 *
	 * @param vendorId
	 * @return
	 * @throws NotFoundException
	 */
	VendorBankDetails getVendorBankDetails(Long vendorId) throws NotFoundException;
}
