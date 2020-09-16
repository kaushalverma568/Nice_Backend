/**
 *
 */
package com.nice.repository;

import java.util.List;

import com.nice.dto.DeliveryLogFilterDTO;
import com.nice.dto.TaskFilterDTO;
import com.nice.model.Task;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 15-Jul-2020
 */
public interface TaskCustomRepository {

	/**
	 *
	 * @param parameterObject
	 * @return
	 */
	Long getTaskCountBasedOnParams(TaskFilterDTO parameterObject);

	/**
	 *
	 * @param parameterObject
	 * @param startIndex
	 * @param pageSize
	 * @return
	 */
	List<Task> getTaskListBasedOnParams(TaskFilterDTO parameterObject, Integer startIndex, Integer pageSize);

	/**
	 * get count of task for delivery log
	 *
	 * @param deliveryLogFilterDTO
	 * @return
	 */
	Long getTaskCountForDeliveryLogBasedOnParams(DeliveryLogFilterDTO deliveryLogFilterDTO);

	/**
	 * get list of task for delivery log
	 *
	 * @param deliveryLogFilterDTO
	 * @param startIndex
	 * @param pageSize
	 * @return
	 */
	List<Task> getTaskListForDeliveryLogBasedOnParams(DeliveryLogFilterDTO deliveryLogFilterDTO, Integer startIndex, Integer pageSize);
}
