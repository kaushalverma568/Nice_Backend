/**
 *
 */
package com.nice.repository;

import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import com.nice.model.DeliveryBoy;
import com.nice.model.Orders;
import com.nice.model.PaymentDetails;
import com.nice.model.Task;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 15-Jul-2020
 */
public interface TaskRepository extends JpaRepository<Task, Long>, TaskCustomRepository {

	/**
	 * get all task by status and task type for delivery boy(Used for replacement pending/attempted ,delivery
	 * pending/attempted)
	 *
	 * @param deliveryBoy
	 * @param status
	 * @param taskType
	 * @return
	 */
	List<Task> findAllByDeliveryBoyAndStatusAndTaskType(DeliveryBoy deliveryBoy, String status, String taskType);

	/**
	 * get all task by status,task type and created at for delivery boy(Used for replacement delivered ,delivery delivered
	 * for today)
	 *
	 * @param deliveryBoy
	 * @param status
	 * @param taskType
	 * @param createdAt
	 * @return
	 */
	List<Task> findAllByDeliveryBoyAndStatusAndTaskTypeAndCreatedAtBetween(DeliveryBoy deliveryBoy, String status, String taskType, Date createdAt,
			Date createdAt1);

	/**
	 * get all task by task type and created at for delivery boy(Used for delivery log detail get pending/delivered task for
	 * date)
	 *
	 * @param deliveryBoy
	 *
	 * @param taskType
	 * @param createdAt
	 * @return
	 */
	List<Task> findAllByDeliveryBoyAndTaskTypeAndUpdatedAtBetweenAndStatusIgnoreCaseIn(DeliveryBoy deliveryBoy, String taskType, Date createdAt,
			Date createdAt1, List<String> status);

	/**
	 * get count by status for delivery boy
	 *
	 * @param deliveryBoy
	 * @param status
	 * @return
	 */
	Integer countByDeliveryBoyAndStatus(DeliveryBoy deliveryBoy, String status);

	/**
	 * get count of order by task status ,task type for delivery boy
	 *
	 * @param deliveryBoy
	 * @param status
	 * @param taskType
	 * @return
	 */
	Integer countByDeliveryBoyAndStatusAndTaskType(DeliveryBoy deliveryBoy, String status, String taskType);

	/**
	 * get count of order by task status and created at for delivery boy
	 *
	 * @param deliveryBoy
	 * @param status
	 * @param date
	 * @return
	 */
	Integer countByDeliveryBoyAndStatusAndCreatedAtBetween(DeliveryBoy deliveryBoy, String status, Date date, Date date1);

	/**
	 * get count of order by task status ,task type and created at for delivery boy
	 *
	 * @param deliveryBoy
	 * @param status
	 * @param taskType
	 * @param date
	 * @return
	 */
	Integer countByDeliveryBoyAndStatusAndTaskTypeAndUpdatedAtBetween(DeliveryBoy deliveryBoy, String status, String taskType, Date date, Date date1);

	/**
	 * @param orderId
	 * @return
	 */
	List<Task> findAllByOrderId(Long orderId);

	/**
	 * @param orderId
	 * @return
	 */
	List<Task> findAllByOrderIdOrderByOrderIdDesc(Long orderId);

	/**
	 * @param orderId
	 * @param allocatedFor
	 * @return
	 */
	Optional<Task> findByOrderAndTaskTypeIgnoreCase(Orders order, String taskType);

	/**
	 * @param orders
	 * @param taskType
	 * @return
	 */
	Long countByOrderAndTaskType(Orders orders, String taskType);

	/**
	 * @param paymentDetails
	 * @return
	 */
	List<Task> findAllByPaymentDetails(PaymentDetails paymentDetails);

	/**
	 * @param deliveryBoyId
	 */
	@Query(value = "Select count(*) from Task t where t.deliveryBoy.id = :deliveryBoyId and t.taskType ='Delivery'")
	Long getCountCartOrderCountForDeliveryPerson(Long deliveryBoyId);

	/**
	 *
	 * @param deliveryBoyId
	 * @return
	 */
	@Query(value = "Select count(*) from Task t where t.deliveryBoy.id = :deliveryBoyId and t.taskType ='Replacement'")
	Long getCountReplacementOrderCountForDeliveryPerson(Long deliveryBoyId);

	/**
	 *
	 * @param deliveryBoyId
	 * @return
	 */
	@Query(value = "Select count(*) from Task t where t.deliveryBoy.id = :deliveryBoyId and t.taskType ='Return'")
	Long getCountReturnOrderCountForDeliveryPerson(Long deliveryBoyId);

	/**
	 *
	 * @param deliveryBoyId
	 * @return
	 */
	@Query(value = "Select sum(t.deliveryCharge) from Task t where t.deliveryBoy.id = :deliveryBoyId")
	Double getTotalDeliveryChargeForDeliveryPerson(Long deliveryBoyId);
}
