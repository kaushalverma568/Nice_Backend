package com.nice.service;

import java.io.IOException;
import java.util.List;

import javax.mail.MessagingException;
import javax.servlet.http.HttpServletResponse;

import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.AssignedOrdersCountDTO;
import com.nice.dto.DashBoardDetailDTO;
import com.nice.dto.DeliveryBoyAccountDetailsDTO;
import com.nice.dto.DeliveryBoyDTO;
import com.nice.dto.DeliveryBoyPersonalDetailsDTO;
import com.nice.dto.DeliveryBoyResponseDTO;
import com.nice.dto.OrderNotificationDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.DeliveryBoy;
import com.nice.model.DeliveryBoyCurrentStatus;

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
	void updatePersonalDetails(DeliveryBoyPersonalDetailsDTO deliveryBoyPersonalDetailsDTO) throws NotFoundException, ValidationException;

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
	 * @param searchKeyword
	 * @return
	 * @throws NotFoundException
	 */
	Page<DeliveryBoy> getDeliveryBoyList(Integer pageNumber, Integer pageSize, Boolean activeRecords, String searchKeyword) throws NotFoundException;

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
	 * validate is log out possible or not
	 *
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void validateBeforeLogout() throws NotFoundException, ValidationException;

	/**
	 * get three nearest delivery boys
	 *
	 * @param orderId
	 * @param vendorId
	 * @return
	 * @throws NotFoundException
	 */
	List<Long> getNextThreeNearestDeliveryBoysFromVendor(Long orderId, Long vendorId) throws NotFoundException;

	/**
	 * set is login flag to true when delivery boy successfully logged in
	 *
	 * @param userName
	 * @throws NotFoundException
	 */
	void updateIsLogin(String userName) throws NotFoundException;

	/**
	 * update the rating of the delivery boy.(provide the rating provided by the
	 * client and deliveryBoyId)
	 *
	 * @param deliveryBoyId
	 * @param ratingByClient
	 * @throws NotFoundException
	 */
	void updateDeliveryBoyRating(Long deliveryBoyId, Double ratingByClient) throws NotFoundException;

	/**
	 * delivery boy with phone number exist
	 *
	 * @param deliveryBoyDTO
	 * @return
	 */
	Boolean isPhoneNumberExists(DeliveryBoyDTO deliveryBoyDTO);

	/**
	 *
	 * @param activeRecords
	 * @param searchKeyword 
	 * @param httpServletResponse
	 * @throws IOException
	 */
	void exportList(Boolean activeRecords, String searchKeyword, HttpServletResponse httpServletResponse) throws IOException;

	/**
	 * get delivery boy current status details
	 *
	 * @param deliveryBoy
	 * @return
	 * @throws NotFoundException
	 */
	DeliveryBoyCurrentStatus getDeliveryBoyCurrentStatusDetail(DeliveryBoy deliveryBoy) throws NotFoundException;

	/**
	 * update delivery boy is available for delivering orders
	 *
	 * @param isAvailable
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateIsAvailable(Boolean isAvailable) throws NotFoundException, ValidationException;

	/**
	 * @param orderId
	 * @param deliveryBoyId
	 * @return
	 * @throws NotFoundException
	 */
	OrderNotificationDTO getOrderDetailInDeliveryBoyAcceptNotification(Long orderId, Long deliveryBoyId) throws NotFoundException;

	/**
	 * get assigned ordres count(regular orders ,return orders,replace orders
	 * separate counts)
	 *
	 * @param deliveryBoyId
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	AssignedOrdersCountDTO getAssignedOrdersCount(Long deliveryBoyId) throws NotFoundException, ValidationException;

	/**
	 * get delivery boy dash board
	 *
	 * @param deliveryBoyId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	DashBoardDetailDTO getDashBoard(Long deliveryBoyId) throws NotFoundException, ValidationException;
}
