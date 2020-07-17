/**
 *
 */
package com.nice.service;

import java.util.Date;
import java.util.List;

import com.nice.dto.TaskDto;
import com.nice.dto.TaskFilterDTO;
import com.nice.dto.TaskResponseDto;
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
	 * @param status
	 * @param taskType
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
	 *
	 * @param taskIds
	 * @param paymentId
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void updatePaymentDetailsInTask(List<Long> taskIds, Long paymentId) throws ValidationException, NotFoundException;

	/**
	 * @param paymentId
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	List<TaskResponseDto> getTaskListFromPayment(Long paymentId) throws ValidationException, NotFoundException;

	/**
	 * 
	 * @param taskId
	 * @return
	 * @throws NotFoundException 
	 */
	Task getTaskDetail(Long taskId) throws NotFoundException;

}
