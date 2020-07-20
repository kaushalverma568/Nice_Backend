/**
 *
 */
package com.nice.repository;

import java.util.Date;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.TaskHistory;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 16-Jul-2020
 */
@Repository
public interface TaskHistoryRepository extends JpaRepository<TaskHistory, Long> {
	/**
	 * get all task by task type and updated at for delivery boy(Used for delivery log detail )
	 *
	 * @param deliveryBoy
	 *
	 * @param taskType
	 * @param createdAt
	 * @return
	 */
	List<TaskHistory> findAllByDeliveryBoyIdAndAndTaskIdAndTaskTypeIgnoreCaseAndStatusIgnoreCase(Long deliveryBoyId, Long taskId, String taskType,
			String status);

	/**
	 * get task history based on param(Used for delivery log detail to get attempted task)
	 *
	 * @param deliveryBoy
	 *
	 * @param taskType
	 * @param createdAt
	 * @return
	 */
	List<TaskHistory> findAllByDeliveryBoyIdAndTaskTypeIgnoreCaseAndCreatedAtBetweenAndStatusIgnoreCase(Long deliveryBoyId, String taskType, Date createdAt,
			Date createdAt1, String status);

}
