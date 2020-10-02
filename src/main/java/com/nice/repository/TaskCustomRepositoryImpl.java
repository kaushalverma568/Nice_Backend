/**
 *
 */
package com.nice.repository;

import java.sql.Date;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.EntityGraph;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Repository;

import com.nice.dto.DeliveryLogFilterDTO;
import com.nice.dto.TaskFilterDTO;
import com.nice.model.Customer;
import com.nice.model.DeliveryBoy;
import com.nice.model.Orders;
import com.nice.model.Task;
import com.nice.model.Vendor;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 13-Apr-2020
 */
@Repository
public class TaskCustomRepositoryImpl implements TaskCustomRepository {

	private static final String CREATED_AT = "createdAt";
	private static final String VENDOR = "vendor";
	private static final String ORDER = "order";
	private static final String DELIVERY_BOY = "deliveryBoy";
	private static final String CUSTOMER = "customer";
	@PersistenceContext
	private EntityManager entityManager;

	@Override
	public Long getTaskCountBasedOnParams(final TaskFilterDTO parameterObject) {
		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();

		CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);

		Root<Task> task = criteriaQuery.from(Task.class);

		Join<Task, Orders> orders = task.join(ORDER, JoinType.INNER);

		List<Predicate> predicates = new ArrayList<>();
		addConditions(parameterObject, criteriaBuilder, task, orders, predicates);

		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(criteriaBuilder.count(task)).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));
		TypedQuery<Long> query = entityManager.createQuery(criteriaQuery);
		return query.getSingleResult();
	}

	/**
	 * @param parameterObject
	 * @param criteriaBuilder
	 * @param task
	 * @param orders
	 * @param predicates
	 */
	private void addConditions(final TaskFilterDTO parameterObject, final CriteriaBuilder criteriaBuilder, final Root<Task> task,
			final Join<Task, Orders> orders, final List<Predicate> predicates) {

		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(parameterObject.getStatusList())) {
			predicates.add(task.get("status").in(parameterObject.getStatusList()));
		}

		if (parameterObject.getOrderId() != null) {
			predicates.add(criteriaBuilder.equal(orders.get("id"), parameterObject.getOrderId()));
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(parameterObject.getStatusListNotIn())) {
			predicates.add(criteriaBuilder.not(task.get("status").in(parameterObject.getStatusListNotIn())));
		}
		if (parameterObject.getTaskType() != null) {
			predicates.add(criteriaBuilder.equal(task.get("taskType"), parameterObject.getTaskType()));
		}
		if (parameterObject.getUpdatedAt() != null) {
			predicates.add(criteriaBuilder.equal(task.get("updatedAt").as(Date.class), parameterObject.getUpdatedAt()));
		}
		if (parameterObject.getCreatedAt() != null) {
			predicates.add(criteriaBuilder.equal(task.get(CREATED_AT).as(Date.class), parameterObject.getCreatedAt()));
		}
		if (parameterObject.getDeliveredDate() != null) {
			predicates.add(criteriaBuilder.equal(task.get("deliveredDate").as(Date.class), parameterObject.getDeliveredDate()));
		}
		if (parameterObject.getOrderDate() != null) {
			predicates.add(criteriaBuilder.equal(orders.get(CREATED_AT).as(Date.class), parameterObject.getOrderDate()));
		}
		if (parameterObject.getDeliveryBoyId() != null) {
			Join<Task, DeliveryBoy> deliveryBoy = task.join(DELIVERY_BOY, JoinType.INNER);
			predicates.add(criteriaBuilder.equal(deliveryBoy.get("id"), parameterObject.getDeliveryBoyId()));
		}
		if (parameterObject.getVendorId() != null) {
			Join<Task, Vendor> vendor = task.join(VENDOR, JoinType.INNER);
			predicates.add(criteriaBuilder.equal(vendor.get("id"), parameterObject.getVendorId()));
		}
		if (parameterObject.getDeliveryBoyPaymentPending() != null) {
			if (parameterObject.getDeliveryBoyPaymentPending().booleanValue()) {
				predicates.add(criteriaBuilder.and(task.get("deliveryBoyPaymentDetails").isNull()));
			} else {
				predicates.add(criteriaBuilder.and(task.get("deliveryBoyPaymentDetails").isNotNull()));
			}
		}
		if (parameterObject.getVendorPaymentPending() != null) {
			if (parameterObject.getVendorPaymentPending().booleanValue()) {
				predicates.add(criteriaBuilder.and(task.get("vendorPaymentDetails").isNull()));
			} else {
				predicates.add(criteriaBuilder.and(task.get("vendorPaymentDetails").isNotNull()));
			}
		}
		if (parameterObject.getOrderDateFrom() != null) {
			if (parameterObject.getOrderDateTo() != null) {
				predicates.add(
						criteriaBuilder.between(orders.get(CREATED_AT).as(Date.class), parameterObject.getOrderDateFrom(), parameterObject.getOrderDateTo()));
			} else {
				predicates.add(criteriaBuilder.greaterThanOrEqualTo(orders.get(CREATED_AT).as(Date.class), parameterObject.getOrderDateFrom()));
			}
		} else {
			if (parameterObject.getOrderDateTo() != null) {
				predicates.add(criteriaBuilder.lessThanOrEqualTo(orders.get(CREATED_AT).as(Date.class), parameterObject.getOrderDateTo()));
			}
		}

		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(parameterObject.getSearchKeyWord())) {
			Predicate predicateForFirstname = criteriaBuilder.like(criteriaBuilder.lower(orders.get("firstName")),
					"%" + parameterObject.getSearchKeyWord().toLowerCase() + "%");
			Predicate predicateForLastname = criteriaBuilder.like(criteriaBuilder.lower(orders.get("lastName")),
					"%" + parameterObject.getSearchKeyWord().toLowerCase() + "%");
			Predicate predicateForOrderId = criteriaBuilder.like(criteriaBuilder.lower(orders.get("id").as(String.class)),
					"%" + parameterObject.getSearchKeyWord().toLowerCase() + "%");
			Predicate predicateForSearch = criteriaBuilder.or(predicateForFirstname, predicateForLastname, predicateForOrderId);
			predicates.add(predicateForSearch);
		}
	}

	@Override
	public List<Task> getTaskListBasedOnParams(final TaskFilterDTO parameterObject, final Integer startIndex, final Integer pageSize) {

		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();

		CriteriaQuery<Task> criteriaQuery = criteriaBuilder.createQuery(Task.class);

		Root<Task> task = criteriaQuery.from(Task.class);

		Join<Task, Orders> orders = task.join(ORDER, JoinType.INNER);

		List<Predicate> predicates = new ArrayList<>();

		addConditions(parameterObject, criteriaBuilder, task, orders, predicates);

		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(task).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));
		criteriaQuery.orderBy(criteriaBuilder.desc(task.get("id")));

		/**
		 * Reducing multiple queries into single queries using graph </br>
		 * It allows defining a template by grouping the related persistence fields which we want to retrieve and lets us choose
		 * the graph type at runtime.
		 */
		EntityGraph<Task> fetchGraph = entityManager.createEntityGraph(Task.class);
		fetchGraph.addSubgraph(ORDER);
		TypedQuery<Task> query = entityManager.createQuery(criteriaQuery).setHint("javax.persistence.loadgraph", fetchGraph);
		if (startIndex != null && pageSize != null) {
			query.setFirstResult(startIndex);
			query.setMaxResults(pageSize);
		}
		return query.getResultList();
	}

	@Override
	public Long getTaskCountForDeliveryLogBasedOnParams(final DeliveryLogFilterDTO deliveryLogFilterDTO) {
		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();

		CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);

		Root<Task> task = criteriaQuery.from(Task.class);

		List<Predicate> predicates = new ArrayList<>();

		Join<Task, Orders> orders = task.join(ORDER, JoinType.INNER);
		addConditionForDeliveryLog(deliveryLogFilterDTO, criteriaBuilder, task, predicates, orders);

		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(criteriaBuilder.count(task)).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));
		TypedQuery<Long> query = entityManager.createQuery(criteriaQuery);
		return query.getSingleResult();
	}

	@Override
	public List<Task> getTaskListForDeliveryLogBasedOnParams(final DeliveryLogFilterDTO deliveryLogFilterDTO, final Integer startIndex,
			final Integer pageSize) {

		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();

		CriteriaQuery<Task> criteriaQuery = criteriaBuilder.createQuery(Task.class);

		Root<Task> task = criteriaQuery.from(Task.class);

		List<Predicate> predicates = new ArrayList<>();
		Join<Task, Orders> orders = task.join(ORDER, JoinType.INNER);
		addConditionForDeliveryLog(deliveryLogFilterDTO, criteriaBuilder, task, predicates, orders);

		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(task).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));
		criteriaQuery.orderBy(criteriaBuilder.desc(task.get("id")));

		/**
		 * Reducing multiple queries into single queries using graph </br>
		 * It allows defining a template by grouping the related persistence fields which we want to retrieve and lets us choose
		 * the graph type at runtime.
		 */
		EntityGraph<Task> fetchGraph = entityManager.createEntityGraph(Task.class);
		fetchGraph.addSubgraph(ORDER);
		TypedQuery<Task> query = entityManager.createQuery(criteriaQuery).setHint("javax.persistence.loadgraph", fetchGraph);
		if (startIndex != null && pageSize != null) {
			query.setFirstResult(startIndex);
			query.setMaxResults(pageSize);
		}
		return query.getResultList();
	}

	private void addConditionForDeliveryLog(final DeliveryLogFilterDTO deliveryLogFilterDTO, final CriteriaBuilder criteriaBuilder, final Root<Task> task,
			final List<Predicate> predicates, final Join<Task, Orders> orders) {

		predicates.add(criteriaBuilder.isNotNull(task.get(DELIVERY_BOY)));
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(deliveryLogFilterDTO.getTaskType())) {
			predicates.add(criteriaBuilder.equal(task.get("taskType"), deliveryLogFilterDTO.getTaskType()));
		}
		if (deliveryLogFilterDTO.getVendorId() != null) {
			Join<Task, Vendor> vendor = task.join(VENDOR, JoinType.INNER);
			predicates.add(criteriaBuilder.equal(vendor.get("id"), deliveryLogFilterDTO.getVendorId()));
		}
		if (deliveryLogFilterDTO.getCustomerId() != null) {
			Join<Orders, Customer> customer = orders.join(CUSTOMER, JoinType.INNER);
			predicates.add(criteriaBuilder.equal(customer.get("id"), deliveryLogFilterDTO.getCustomerId()));
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(deliveryLogFilterDTO.getSearchKeyword())) {
			Join<Task, DeliveryBoy> deliveryBoy = task.join(DELIVERY_BOY, JoinType.INNER);
			if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
				Predicate predicateForFirstname = criteriaBuilder.like(criteriaBuilder.lower(deliveryBoy.get("firstNameEnglish")),
						"%" + deliveryLogFilterDTO.getSearchKeyword().toLowerCase() + "%");
				Predicate predicateForLastname = criteriaBuilder.like(criteriaBuilder.lower(deliveryBoy.get("lastNameEnglish")),
						"%" + deliveryLogFilterDTO.getSearchKeyword().toLowerCase() + "%");
				Predicate predicateForSearch = criteriaBuilder.or(predicateForFirstname, predicateForLastname);
				predicates.add(predicateForSearch);
			} else {
				Predicate predicateForFirstname = criteriaBuilder.like(criteriaBuilder.lower(deliveryBoy.get("firstNameArabic")),
						"%" + deliveryLogFilterDTO.getSearchKeyword().toLowerCase() + "%");
				Predicate predicateForLastname = criteriaBuilder.like(criteriaBuilder.lower(deliveryBoy.get("lastNameArabic")),
						"%" + deliveryLogFilterDTO.getSearchKeyword().toLowerCase() + "%");
				Predicate predicateForSearch = criteriaBuilder.or(predicateForFirstname, predicateForLastname);
				predicates.add(predicateForSearch);
			}
		}
		if (deliveryLogFilterDTO.getFromDate() != null) {
			if (deliveryLogFilterDTO.getToDate() != null) {
				predicates.add(
						criteriaBuilder.between(task.get(CREATED_AT).as(Date.class), deliveryLogFilterDTO.getFromDate(), deliveryLogFilterDTO.getToDate()));
			} else {
				predicates.add(criteriaBuilder.equal(task.get(CREATED_AT).as(Date.class), deliveryLogFilterDTO.getFromDate()));
			}
		} else {
			deliveryLogFilterDTO.setFromDate(new java.util.Date());
			predicates.add(criteriaBuilder.equal(task.get(CREATED_AT).as(Date.class), deliveryLogFilterDTO.getFromDate()));
		}
	}

}
