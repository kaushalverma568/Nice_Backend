package com.nice.service;

import java.io.IOException;
import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.HesabePaymentDTO;
import com.nice.dto.VendorAppResponseDTO;
import com.nice.dto.VendorBankDetailsDTO;
import com.nice.dto.VendorBasicDetailDTO;
import com.nice.dto.VendorDTO;
import com.nice.dto.VendorFilterDTO;
import com.nice.dto.VendorListFilterDTO;
import com.nice.dto.VendorResponseDTO;
import com.nice.dto.VendorRestaurantDetailsDTO;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.Vendor;
import com.nice.model.VendorBankDetails;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 25, 2020
 */

public interface VendorService {
	/**
	 * persist vendor object
	 *
	 * @param vendorDTO
	 * @param userId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void addVendor(VendorDTO vendorDTO) throws ValidationException, NotFoundException;

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
	 * check vendor duplication on based on email and returning Boolean value.
	 *
	 * @param vendorDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Boolean isVendorExists(VendorDTO vendorDTO);

	/**
	 * check vendor duplication on based on contact and returning Boolean value.
	 *
	 * @param vendorDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Boolean isVendorContactExists(VendorDTO vendorDTO);

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
	 * Update bank details
	 *
	 * @param vendorBankDetailsDTO
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateBankDetails(VendorBankDetailsDTO vendorBankDetailsDTO) throws NotFoundException, ValidationException;

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
	 * update vendor's personal details
	 *
	 * @param vendorDTO
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updatePersonalDetails(VendorDTO vendorDTO) throws NotFoundException, ValidationException;

	/**
	 * generates hesabe paymentLink
	 *
	 * @param vendorId
	 * @param subscriptionPlanId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	String updateSubscriptionPlanForVendor(Long vendorId, Long subscriptionPlanId) throws NotFoundException, ValidationException;

	/**
	 * update restaurant details
	 *
	 * @param vendorRestaurantDetailsDTO
	 * @param userId
	 * @throws NotFoundException
	 * @throws ValidationException
	 * @throws FileOperationException
	 */
	void updateRestaurantDetails(VendorRestaurantDetailsDTO vendorRestaurantDetailsDTO, MultipartFile storeImage, MultipartFile storeDetailImage,
			MultipartFile featuredImage) throws NotFoundException, ValidationException, FileOperationException;

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

	/**
	 * vendor list for customer app
	 *
	 * @param vendorListFilterDTO
	 * @param pageSize
	 * @param startIndex
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	List<VendorAppResponseDTO> getVendorListForApp(VendorListFilterDTO vendorListFilterDTO, Integer startIndex, Integer pageSize)
			throws ValidationException, NotFoundException;

	/**
	 * scheduler method for expire subscription
	 *
	 * @param runDate
	 */
	List<Long> runVendorSubscriptionExpireScheduler(Date runDate);

	/**
	 * scheduler method to send email regarding subscription expire
	 *
	 * @param runDate
	 */
	void runVendorSubscriptionExpireReminderScheduler(Date runDate);

	/**
	 * change vendor status
	 *
	 * @param vendorId
	 * @param newStatus
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	String changeVendorStatus(Long vendorId, String newStatus) throws NotFoundException, ValidationException;

	/**
	 * @param activeRecords
	 * @param httpServletResponse
	 * @throws IOException
	 * @throws FileNotFoundException
	 * @throws ValidationException
	 */
	void exportVendorList(VendorFilterDTO vendorFilterDTO, HttpServletResponse httpServletResponse) throws FileNotFoundException, ValidationException;

	/**
	 * verify vendor contact
	 *
	 * @param vendorId
	 * @param otp
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void verifyVendorContact(Long vendorId, String otp) throws NotFoundException, ValidationException;

	/**
	 * delete vendor image by type
	 *
	 * @param vendorId
	 * @param type
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void deleteVendorImageByType(Long vendorId, String type) throws NotFoundException, ValidationException;

	/**
	 * set vendor is featured
	 *
	 * @param vendorId
	 * @param active
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void changeStatusOfIsFeaturedVendor(Long vendorId, Boolean active) throws NotFoundException, ValidationException;

	/**
	 * get vendor basic details by id
	 *
	 * @param vendorId
	 * @return
	 * @throws NotFoundException
	 */
	VendorBasicDetailDTO getVendorBasicDetailById(Long vendorId) throws NotFoundException;

	/**
	 * send email for change vendor status
	 *
	 * @param vendorId
	 * @throws NotFoundException
	 */
	void sendEmailForChangeVendorStatus(Long vendorId) throws NotFoundException;

	/**
	 * vendor count for app list
	 *
	 * @param vendorListFilterDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Long getVendorCountForCustomerBasedOnParams(VendorListFilterDTO vendorListFilterDTO) throws ValidationException, NotFoundException;

	/**
	 * hesabe payment response for add vendor subscription
	 *
	 * @param response
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	boolean checkPaymentTransactionHesabe(HesabePaymentDTO response) throws NotFoundException, ValidationException;

	/**
	 * get vendor details for app with distance
	 *
	 * @param vendorFilterDTO
	 * @param vendorId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	VendorResponseDTO getVendorDetailForApp(Long vendorId, VendorListFilterDTO vendorListFilterDTO) throws ValidationException, NotFoundException;
}
