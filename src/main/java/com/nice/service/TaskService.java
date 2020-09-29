/**
 *
 */
package com.nice.service;

import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpServletResponse;

import com.nice.dto.DeliveryBoyOrderCountDto;
import com.nice.dto.DeliveryLogDTO;
import com.nice.dto.DeliveryLogFilterDTO;
import com.nice.dto.TaskDto;
import com.nice.dto.TaskFilterDTO;
import com.nice.dto.TaskResponseDto;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.DeliveryBoy;
import com.nice.model.Orders;
import com.nice.model.Task;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 12-Apr-2020
 */
public interface TaskService {

	/**
	 * @param taskDto
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Task createTask(TaskDto taskDto) throws NotFoundException, ValidationException;

	/**
	 * @param taskId
	 * @param taskStatus
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void changeTaskStatus(Long taskId, String taskStatus) throws ValidationException, NotFoundException;

	/**
	 * get orders count according to status and taskType for delivery boy
	 *
	 * @param deliveryBoy
	 * @param taskType
	 * @param status
	 * @return
	 */
	Integer getOrdersCountAccordingToStatusAndTaskTypeForDeliveryBoy(DeliveryBoy deliveryBoy, String status, String taskType);

	/**
	 * get orders count according to status ,taskType and date for delivery boy(get today's delivered /replaced count)
	 *
	 * @param deliveryBoy
	 * @param status
	 * @param taskType
	 * @param date
	 * @return
	 */
	Integer getOrdersCountAccordingToStatusAndTaskTypeAndDateForDeliveryBoy(DeliveryBoy deliveryBoy, String status, String taskType, Date date);

	/**
	 * get task list count based on parameters (for today's delivered/replaced order list for delivery boy created at will
	 * be current date)
	 *
	 * @param taskFilterDTO
	 * @return
	 */
	Long getTaskCountBasedOnParams(TaskFilterDTO taskFilterDTO);

	/**
	 * get task list based on parameters
	 *
	 * @param taskFilterDTO
	 * @param startIndex
	 * @param pageSize
	 * @return
	 */
	List<Task> getTaskListBasedOnParams(TaskFilterDTO taskFilterDTO, Integer startIndex, Integer pageSize);

	/**
	 * @param orderId
	 */
	List<Task> getTaskListForOrderId(Long orderId);

	/**
	 * @param orderId
	 * @param name
	 * @return
	 * @throws NotFoundException
	 */
	Task getTaskForOrderIdAndAllocatedFor(Orders order, String name) throws NotFoundException;

	/**
	 * send email for order delivery confirmation
	 *
	 * @param orderId
	 * @throws NotFoundException
	 */
	void sendEmailForOrderDeliveryConfirmation(Long orderId) throws NotFoundException;

	/**
	 * send sms to customer when delivery attempted by delivery boy
	 *
	 * @param id
	 * @param taskId
	 * @throws NotFoundException
	 */
	void sendSmsWhenDeliveryBoyAttemptDelivery(Long orderId, Long taskId) throws NotFoundException;

	/**
	 * get delivery/replacement order list for delivery boy for date
	 *
	 * @param deliveryBoy
	 * @param taskType
	 * @param date
	 * @param status
	 * @param status1
	 * @return
	 */
	List<Task> getOrderListForDeliveryBoyBasedOnTaskTypeAndDate(DeliveryBoy deliveryBoy, String taskType, Date date, List<String> status);

	/**
	 * @param taskId
	 * @return
	 * @throws NotFoundException
	 */
	TaskResponseDto getTaskDetails(Long taskId) throws NotFoundException;

	/**
	 * @param taskIds
	 * @param paymentId
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void updateDeliveryBoyPaymentDetailsInTask(List<Long> taskIds, Long paymentId) throws ValidationException, NotFoundException;

	/**
	 * Get the orders linked to payout Id based on user type
	 *
	 * @param paymentId
	 * @param userType
	 * @return
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	List<TaskResponseDto> getTaskListFromPayment(Long paymentId, String userType) throws ValidationException, NotFoundException;

	/**
	 * @param taskId
	 * @return
	 * @throws NotFoundException
	 */
	Task getTaskDetail(Long taskId) throws NotFoundException;

	/**
	 * update task status to pick up on way(for delivery and return order status will be pick up on way and for replacement,
	 * status will be replace customer pick up on way)
	 *
	 * @param taskId
	 * @param status
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	void updateStatusToPickOnWay(Long taskId, String status) throws NotFoundException, ValidationException;

	/**
	 * update task status to delivered
	 *
	 * @param taskId
	 * @return TODO
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Task completeTask(Long taskId) throws NotFoundException, ValidationException;

	/**
	 * @param deliveryBoyId
	 * @return
	 */
	DeliveryBoyOrderCountDto getTaskTypeWiseCountForPaymentDetailsId(Long deliveryBoyId);

	/**
	 * get task list for delivery log
	 *
	 * @param deliveryLogFilterDTO
	 * @param startIndex
	 * @param pageSize
	 * @return
	 */
	List<DeliveryLogDTO> getTaskListForDeliveryLog(DeliveryLogFilterDTO deliveryLogFilterDTO, Integer startIndex, Integer pageSize);

	/**
	 * get task count for delivery log
	 *
	 * @param deliveryLogFilterDTO
	 * @return
	 */
	Long getTaskCountForDeliveryLog(DeliveryLogFilterDTO deliveryLogFilterDTO);

	/**
	 * export delivery log based on params
	 *
	 * @param deliveryLogFilterDTO
	 * @param httpServletResponse
	 * @throws FileNotFoundException
	 * @throws ValidationException
	 */
	void exportDeliveryLogList(DeliveryLogFilterDTO deliveryLogFilterDTO, HttpServletResponse httpServletResponse)
			throws FileNotFoundException, ValidationException;

	/**
	 * Get task list for payout
	 *
	 * @param taskFilterDTO
	 * @param startIndex
	 * @param pageSize
	 * @return
	 */
	List<Task> getTaskListForPayout(TaskFilterDTO taskFilterDTO, Integer startIndex, Integer pageSize);

	/**
	 * Export task list for payout
	 *
	 * @param httpServletResponse
	 * @param taskFilterDTO
	 * @throws FileOperationException
	 */
	void exportTaskListForPayout(HttpServletResponse httpServletResponse, TaskFilterDTO taskFilterDTO) throws FileOperationException;

	/**
	 * @param pushNotificationType
	 * @param task
	 * @throws NotFoundException
	 */
	void sendOrderDeliveryPushNotification(String pushNotificationType, Task task) throws NotFoundException;
}
