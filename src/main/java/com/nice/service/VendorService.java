package com.nice.service;

import java.io.IOException;
import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.EmailUpdateDTO;
import com.nice.dto.VendorBankDetailsDTO;
import com.nice.dto.VendorDTO;
import com.nice.dto.VendorFilterDTO;
import com.nice.dto.VendorListFilterDTO;
import com.nice.dto.VendorResponseDTO;
import com.nice.dto.VendorRestaurantDetailsDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Vendor;
import com.nice.model.VendorBankDetails;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 25, 2020
 */

public interface VendorService {
	/**
	 * persist vendor object
	 *
	 * @param  vendorDTO
	 * @param  profilePicture
	 * @param  userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addVendor(VendorDTO vendorDTO, MultipartFile profilePicture) throws ValidationException, NotFoundException;

	/**
	 * get DTO object of vendor
	 *
	 * @param  vendorId
	 * @return
	 * @throws NotFoundException
	 */
	VendorResponseDTO getVendor(final Long vendorId) throws NotFoundException;

	/**
	 * change status of vendor (active/deActive)
	 *
	 * @param  vendorId
	 * @param  isActive
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	String changeStatus(Long vendorId, Boolean isActive) throws NotFoundException, ValidationException;

	/**
	 * check vendor duplication on based on email and returning Boolean value.
	 *
	 * @param  vendorDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Boolean isVendorExists(VendorDTO vendorDTO);

	/**
	 * check vendor duplication on based on contact and returning Boolean value.
	 *
	 * @param  vendorDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Boolean isVendorContactExists(VendorDTO vendorDTO);

	/**
	 * get detail object of vendor
	 *
	 * @param  vendorId
	 * @return
	 * @throws NotFoundException
	 */
	Vendor getVendorDetail(Long vendorId) throws NotFoundException;

	/**
	 * check is email exist in user login at the time of create or update delivery boy
	 *
	 * @param  vendorDTO
	 * @return
	 */
	Boolean isUserLoginExists(VendorDTO vendorDTO);

	/**
	 * upload profile picture
	 *
	 * @param  profilePicture
	 * @param  vendorId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void uploadProfilePicture(MultipartFile profilePicture, Long vendorId) throws NotFoundException, ValidationException;

	/**
	 * Update bank details
	 *
	 * @param  vendorBankDetailsDTO
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateBankDetails(VendorBankDetailsDTO vendorBankDetailsDTO) throws NotFoundException, ValidationException;

	/**
	 * get page of delivery boy by parameters
	 *
	 * @param  pageNumber
	 * @param  pageSize
	 * @param  activeRecords
	 * @param  isEmailVerified
	 * @return
	 * @throws NotFoundException
	 */
	Page<Vendor> getVendorList(Integer pageNumber, Integer pageSize, Boolean activeRecords, Boolean isEmailVerified) throws NotFoundException;

	/**
	 * update email verified status of delivery boy
	 *
	 * @param  vendorId
	 * @throws NotFoundException
	 */
	void verifyEmail(Long vendorId) throws NotFoundException;

	/**
	 * remove profile picture
	 *
	 * @param  vendorId
	 * @throws NotFoundException
	 */
	void deleteProfilePicture(Long vendorId) throws NotFoundException;

	/**
	 * update vendor's personal details
	 *
	 * @param  vendorDTO
	 * @param  userId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updatePersonalDetails(VendorDTO vendorDTO, MultipartFile profilePicture) throws NotFoundException, ValidationException;

	/**
	 * add update subscription plan
	 *
	 * @param  vendorId
	 * @param  subscriptionPlanId
	 * @param  userId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void addUpdateSubscriptionPlan(Long vendorId, Long subscriptionPlanId) throws NotFoundException, ValidationException;

	/**
	 * update restaurant details
	 *
	 * @param  vendorRestaurantDetailsDTO
	 * @param  userId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateRestaurantDetails(VendorRestaurantDetailsDTO vendorRestaurantDetailsDTO) throws NotFoundException, ValidationException;

	/**
	 * update order service is unable or not for the vendor
	 *
	 * @param  vendorId
	 * @param  isOrderServiceEnable
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void updateOrderServiceEnableForVendor(Long vendorId, Boolean isOrderServiceEnable) throws ValidationException, NotFoundException;

	/**
	 * get vendor count based on filter
	 *
	 * @param  vendorFilterDTO
	 * @return
	 */
	Long getVendorCountBasedOnParams(VendorFilterDTO vendorFilterDTO);

	/**
	 * get vendor list based on parameters
	 *
	 * @param  startIndex
	 * @param  pageSize
	 * @param  vendorFilterDTO
	 * @return
	 * @throws ValidationException
	 */
	List<Vendor> getVendorListBasedOnParams(Integer startIndex, Integer pageSize, VendorFilterDTO vendorFilterDTO) throws ValidationException;

	/**
	 * get vendor bank detail
	 *
	 * @param  vendorId
	 * @return
	 * @throws NotFoundException
	 */
	VendorBankDetails getVendorBankDetails(Long vendorId) throws NotFoundException;

	/**
	 * vendor list for customer app
	 *
	 * @param  vendorListFilterDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	List<VendorResponseDTO> getVendorListForApp(VendorListFilterDTO vendorListFilterDTO) throws ValidationException, NotFoundException;

	/**
	 * scheduler method for expire subscription
	 *
	 * @param runDate
	 */
	void runVendorSubscriptionExpireScheduler(Date runDate);

	/**
	 * scheduler method to send email regarding subscription expire
	 *
	 * @param runDate
	 */
	void runVendorSubscriptionExpireReminderScheduler(Date runDate);

	/**
	 * change vendor status
	 *
	 * @param  vendorId
	 * @param  newStatus
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	String changeVendorStatus(Long vendorId, String newStatus) throws NotFoundException, ValidationException;

	/**
	 * change email for vendor
	 *
	 * @param  vendorId
	 * @param  emailUpdateDTO
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	String changeVendorEmail(Long vendorId, EmailUpdateDTO emailUpdateDTO) throws NotFoundException, ValidationException;

	/**
	 * generate otp to change email
	 *
	 * @param  email
	 * @param  vendorId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	String generateOTPForChangeEmail(String email, Long vendorId) throws NotFoundException, ValidationException;

	/**
	 * @param  activeRecords
	 * @param  httpServletResponse
	 * @throws IOException
	 */
	void exportVendorList(Boolean activeRecords, HttpServletResponse httpServletResponse) throws IOException;

	/**
	 * generate OTP for change contact
	 *
	 * @param  contactNo
	 * @param  vendorId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	String generateOTPForChangeContact(String contactNo, Long vendorId) throws NotFoundException, ValidationException;

	/**
	 * change vendor contactNo
	 * 
	 * @param  vendorId
	 * @param  contactNo
	 * @param  otp
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void changeVendorContact(Long vendorId, String contactNo, String otp) throws NotFoundException, ValidationException;
}
