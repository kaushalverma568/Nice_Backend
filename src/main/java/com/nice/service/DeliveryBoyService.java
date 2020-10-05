package com.nice.service;

import java.util.List;

import javax.mail.MessagingException;
import javax.servlet.http.HttpServletResponse;

import org.springframework.web.multipart.MultipartFile;

import com.nice.dto.DashBoardDetailDTO;
import com.nice.dto.DeliveryBoyAccountDetailsDTO;
import com.nice.dto.DeliveryBoyDTO;
import com.nice.dto.DeliveryBoyFilterDTO;
import com.nice.dto.DeliveryBoyPersonalDetailsDTO;
import com.nice.dto.DeliveryBoyResponseDTO;
import com.nice.dto.OrdersCountDTO;
import com.nice.dto.OrdersDetailDTOForDeliveryBoy;
import com.nice.dto.OrdersListDTOForDeliveryBoy;
import com.nice.dto.TaskFilterDTO;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.DeliveryBoy;
import com.nice.model.DeliveryBoyCurrentStatus;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : Jun 18, 2020
 */

public interface DeliveryBoyService {
	/**
	 * persist deliveryBoy object
	 *
	 * @param  deliveryBoyDTO
	 * @param  profilePicture
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 * @throws FileOperationException
	 * @throws MessagingException
	 */
	DeliveryBoyResponseDTO addDeliveryBoy(DeliveryBoyDTO deliveryBoyDTO, MultipartFile profilePicture)
			throws ValidationException, NotFoundException, FileOperationException;

	/**
	 * get DTO object of deliveryBoy
	 *
	 * @param  deliveryBoyId
	 * @return
	 * @throws NotFoundException
	 */
	DeliveryBoyResponseDTO getDeliveryBoy(final Long deliveryBoyId) throws NotFoundException;

	/**
	 * change status of deliveryBoy (active/deActive)
	 *
	 * @param  deliveryBoyId
	 * @param  isActive
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	String changeStatus(Long deliveryBoyId, Boolean isActive) throws NotFoundException, ValidationException;

	/**
	 * check deliveryBoy duplication and returning Boolean value.
	 *
	 * @param  deliveryBoyDTO
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	Boolean isDeliveryBoyExists(DeliveryBoyDTO deliveryBoyDTO);

	/**
	 * get detail object of deliveryBoy
	 *
	 * @param  deliveryBoyId
	 * @return
	 * @throws NotFoundException
	 */
	DeliveryBoy getDeliveryBoyDetail(Long deliveryBoyId) throws NotFoundException;

	/**
	 * update profile picture
	 *
	 * @param  profilePicture
	 * @param  deliveryBoyId
	 * @throws NotFoundException
	 * @throws ValidationException
	 * @throws FileOperationException
	 */
	void updateProfilePicture(MultipartFile profilePicture) throws NotFoundException, ValidationException, FileOperationException;

	/**
	 * Update personal details
	 *
	 * @param  deliveryBoyPersonalDetailsDTO
	 * @return
	 * @throws ValidationException
	 */
	void updatePersonalDetails(DeliveryBoyPersonalDetailsDTO deliveryBoyPersonalDetailsDTO) throws NotFoundException, ValidationException;

	/**
	 * Update account details
	 *
	 * @param  deliveryBoyAccountDetailsDTO
	 * @throws NotFoundException
	 */
	void updateAccountDetails(DeliveryBoyAccountDetailsDTO deliveryBoyAccountDetailsDTO) throws NotFoundException;

	/**
	 * update email verified status of delivery boy
	 *
	 * @param  deliveryBoyId
	 * @throws NotFoundException
	 */
	void verifyEmail(Long deliveryBoyId) throws NotFoundException;

	/**
	 * accept order
	 *
	 * @param  orderId
	 * @param  taskType
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void acceptOrder(Long orderId, String taskType) throws NotFoundException, ValidationException;

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
	 * @param  orderId
	 * @param  vendorId
	 * @return
	 * @throws NotFoundException
	 */
	List<Long> getNextThreeNearestDeliveryBoysFromVendor(Long orderId, Long vendorId) throws NotFoundException;

	/**
	 * set is login flag to true when delivery boy successfully logged in
	 *
	 * @param  userName
	 * @throws NotFoundException
	 */
	void updateIsLogin(String userName) throws NotFoundException;

	/**
	 * update the rating of the delivery boy.(provide the rating provided by the client and deliveryBoyId)
	 *
	 * @param  deliveryBoyId
	 * @param  ratingByClient
	 * @throws NotFoundException
	 */
	void updateDeliveryBoyRating(Long deliveryBoyId, Double ratingByClient) throws NotFoundException;

	/**
	 * delivery boy with phone number exist
	 *
	 * @param  deliveryBoyDTO
	 * @return
	 */
	Boolean isPhoneNumberExists(DeliveryBoyDTO deliveryBoyDTO);

	/**
	 * @param  deliveryBoyFilterDTO
	 * @param  httpServletResponse
	 * @throws FileNotFoundException
	 * @throws ValidationException
	 */
	void exportList(DeliveryBoyFilterDTO deliveryBoyFilterDTO, HttpServletResponse httpServletResponse) throws FileNotFoundException, ValidationException;

	/**
	 * get delivery boy current status details
	 *
	 * @param  deliveryBoy
	 * @return
	 * @throws NotFoundException
	 */
	DeliveryBoyCurrentStatus getDeliveryBoyCurrentStatusDetail(DeliveryBoy deliveryBoy) throws NotFoundException;

	/**
	 * update delivery boy is available for delivering orders
	 *
	 * @param  isAvailable
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateIsAvailable(Boolean isAvailable) throws NotFoundException, ValidationException;

	/**
	 * Get Order detail in delivery boy accept notification
	 *
	 * @param  orderId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	OrdersDetailDTOForDeliveryBoy getOrderDetailInDeliveryBoyAcceptNotification(Long orderId) throws NotFoundException, ValidationException;

	/**
	 * get assigned ordres count(regular orders ,return orders,replace orders separate counts) get assigned/delivered orders
	 * count(regular orders ,return orders,replace orders separate counts)
	 *
	 * @param  deliveryBoyId
	 * @param  taskFilterDTO
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	OrdersCountDTO getOrdersCount(TaskFilterDTO taskFilterDTO) throws NotFoundException, ValidationException;

	/**
	 * get delivery boy dash board
	 *
	 * @param  deliveryBoyId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	DashBoardDetailDTO getDashBoard() throws NotFoundException, ValidationException;

	/**
	 * get delivery boy count based on param
	 *
	 * @param  deliveryBoyFilterDTO
	 * @return
	 */
	Long getDeliveryBoyCountBasedOnParams(DeliveryBoyFilterDTO deliveryBoyFilterDTO);

	/**
	 * get list of delivery boy by parameters
	 *
	 * @param  startIndex
	 * @param  pageSize
	 * @param  deliveryBoyFilterDTO
	 * @return
	 * @throws ValidationException
	 */
	List<DeliveryBoy> getDeliveryBoyListBasedOnParams(Integer startIndex, Integer pageSize, DeliveryBoyFilterDTO deliveryBoyFilterDTO)
			throws ValidationException;

	/**
	 * @param  deliveryBoyId
	 * @param  startIndex
	 * @param  pageSize
	 * @param  taskFilterDTO
	 * @return
	 * @throws NotFoundException
	 */
	List<OrdersListDTOForDeliveryBoy> getOrdersList(Long deliveryBoyId, Integer startIndex, Integer pageSize, TaskFilterDTO taskFilterDTO)
			throws NotFoundException;

	/**
	 * Get order details for delivery boy
	 *
	 * @param  taskId
	 * @param  orderId
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	OrdersDetailDTOForDeliveryBoy getOrderDetails(Long taskId, Long orderId) throws NotFoundException, ValidationException;

	/**
	 * get delivery boy id from token : This will use internally
	 *
	 * @return
	 * @throws ValidationException
	 */
	Long getDeliveryBoyIdFromToken() throws ValidationException;

	/**
	 * get on field Delivery boys
	 */
	Long getCountOfOnFieldDeliveryBoy();

	/**
	 * get new registered delivery boy count
	 *
	 * @return
	 */
	Long getCountOfNewDeliveryBoys();

	/**
	 * @param  acceptOrderPushNotificationCustomer
	 * @param  orderId
	 * @throws NotFoundException
	 */
	void sendPushNotification(String acceptOrderPushNotificationCustomer, Long orderId) throws NotFoundException;

	/**
	 * Send email after account activation
	 *
	 * @param deliveryBoyId
	 */
	void sendEmailAfterAccountActivation(Long deliveryBoyId);

	Long getAllDeliveryBoyCount();

	/**
	 * @param  userLogin
	 * @param  deliveryBoy
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void sendOtpForEmailVerification(DeliveryBoyResponseDTO deliveryBoyResponseDTO) throws NotFoundException, ValidationException;
}
