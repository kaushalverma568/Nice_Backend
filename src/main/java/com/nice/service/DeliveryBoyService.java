package com.nice.service;

import javax.mail.MessagingException;

import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.DeliveryBoyAccountDetailsDTO;
import com.nice.dto.DeliveryBoyDTO;
import com.nice.dto.DeliveryBoyPersonalDetailsDTO;
import com.nice.dto.DeliveryBoyResponseDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.DeliveryBoy;

/**
 *
 * @author : Kody Technolab Pvt. Ltd.
 * @date : Jun 18, 2020
 */

public interface DeliveryBoyService {
	/**
	 * persist deliveryBoy object
	 *
	 * @param deliveryBoyDTO
	 * @param profilePicture
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws MessagingException
	 */
	void addDeliveryBoy(DeliveryBoyDTO deliveryBoyDTO, MultipartFile profilePicture) throws ValidationException, NotFoundException;

	/**
	 * get DTO object of deliveryBoy
	 *
	 * @param deliveryBoyId
	 * @return
	 * @throws NotFoundException
	 */
	DeliveryBoyResponseDTO getDeliveryBoy(final Long deliveryBoyId) throws NotFoundException;

	/**
	 * change status of deliveryBoy (active/deActive)
	 *
	 * @param deliveryBoyId
	 * @param isActive
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	String changeStatus(Long deliveryBoyId, Boolean isActive) throws NotFoundException, ValidationException;

	/**
	 * check deliveryBoy duplication and returning Boolean value.
	 *
	 * @param deliveryBoyDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Boolean isDeliveryBoyExists(DeliveryBoyDTO deliveryBoyDTO);

	/**
	 * get detail object of deliveryBoy
	 *
	 * @param deliveryBoyId
	 * @return
	 * @throws NotFoundException
	 */
	DeliveryBoy getDeliveryBoyDetail(Long deliveryBoyId) throws NotFoundException;

	/**
	 * check is email exist in user login at the time of create or update delivery
	 * boy
	 *
	 * @param deliveryBoyDTO
	 * @return
	 */
	Boolean isUserLoginExists(DeliveryBoyDTO deliveryBoyDTO);

	/**
	 * update profile picture
	 *
	 * @param profilePicture
	 * @param deliveryBoyId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateProfilePicture(MultipartFile profilePicture, Long deliveryBoyId) throws NotFoundException, ValidationException;

	/**
	 * Update personal details
	 *
	 * @param deliveryBoyPersonalDetailsDTO
	 * @return
	 * @throws ValidationException
	 */
	String updatePersonalDetails(DeliveryBoyPersonalDetailsDTO deliveryBoyPersonalDetailsDTO) throws NotFoundException, ValidationException;

	/**
	 * Update account details
	 *
	 * @param deliveryBoyAccountDetailsDTO
	 * @throws NotFoundException
	 */
	void updateAccountDetails(DeliveryBoyAccountDetailsDTO deliveryBoyAccountDetailsDTO) throws NotFoundException;

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
	Page<DeliveryBoy> getDeliveryBoyList(Integer pageNumber, Integer pageSize, Boolean activeRecords, Boolean isEmailVerified) throws NotFoundException;

	/**
	 * update email verified status of delivery boy
	 *
	 * @param deliveryBoyId
	 * @throws NotFoundException
	 */
	void verifyEmail(Long deliveryBoyId) throws NotFoundException;

	/**
	 * accept order
	 *
	 * @param deliveryBoyId
	 * @param orderId
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void acceptOrder(Long deliveryBoyId, Long orderId) throws NotFoundException, ValidationException;

	/**
	 * send notification to delivery boys for accepting order
	 *
	 * @param orderId
	 * @param vendorId
	 * @throws NotFoundException
	 * @throws InterruptedException
	 * @throws ValidationException
	 */
	void sendNotificationToDeliveryBoysForAcceptingOrder(Long orderId, Long vendorId) throws NotFoundException, InterruptedException, ValidationException;

	/**
	 * validate is log out possible or not
	 *
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void validateBeforeLogout() throws NotFoundException, ValidationException;
}
