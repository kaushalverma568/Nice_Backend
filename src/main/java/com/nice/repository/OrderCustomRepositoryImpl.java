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

import org.springframework.stereotype.Repository;

import com.nice.constant.PaymentMode;
import com.nice.dto.OrderListFilterDto;
import com.nice.model.Customer;
import com.nice.model.DeliveryBoy;
import com.nice.model.Orders;
import com.nice.model.Vendor;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 13-Apr-2020
 */
@Repository
public class OrderCustomRepositoryImpl implements OrderCustomRepository {

	/**
	 *
	 */
	private static final String PAYMENT_MODE = "paymentMode";
	/**
	 *
	 */
	private static final String ORDER_STATUS = "orderStatus";
	private static final String VENDOR_PARAM = "vendor";
	private static final String DELIVERY_BOY = "deliveryBoy";
	private static final String CUSTOMER = "customer";
	private static final String REPLACEMENT_DELIVERY_BOY = "replacementDeliveryBoy";
	private static final String DELIVERY_DATE = "deliveryDate";

	@PersistenceContext
	private EntityManager entityManager;

	@Override
	public List<Orders> getOrderListBasedOnParams(final Integer startIndex, final Integer pageSize, final OrderListFilterDto orderListFilterDto) {

		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();

		CriteriaQuery<Orders> criteriaQuery = criteriaBuilder.createQuery(Orders.class);

		/**
		 * Create and add a query root corresponding to the city.It is similar to the FROM clause in a JPQL query.
		 */
		Root<Orders> orders = criteriaQuery.from(Orders.class);

		List<Predicate> predicates = new ArrayList<>();

		addConditions(orderListFilterDto, criteriaBuilder, orders, predicates);
		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(orders).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));
		criteriaQuery.orderBy(criteriaBuilder.desc(orders.get("id")));
		/**
		 * Reducing multiple queries into single queries using graph </br>
		 * It allows defining a template by grouping the related persistence fields which we want to retrieve and lets us choose
		 * the graph type at runtime.
		 */
		EntityGraph<Orders> fetchGraph = entityManager.createEntityGraph(Orders.class);
		fetchGraph.addSubgraph(VENDOR_PARAM);
		fetchGraph.addSubgraph(DELIVERY_BOY);
		TypedQuery<Orders> query = entityManager.createQuery(criteriaQuery).setHint("javax.persistence.loadgraph", fetchGraph);
		if (startIndex != null && pageSize != null) {
			query.setFirstResult(startIndex);
			query.setMaxResults(pageSize);
		}

		return query.getResultList();
	}

	@Override
	public Long getOrderCountBasedOnParams(final OrderListFilterDto orderListFilterDto) {
		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();

		CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);

		/**
		 * Create and add a query root corresponding to the city.It is similar to the FROM clause in a JPQL query.
		 */
		Root<Orders> orders = criteriaQuery.from(Orders.class);

		List<Predicate> predicates = new ArrayList<>();

		addConditions(orderListFilterDto, criteriaBuilder, orders, predicates);

		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(criteriaBuilder.count(orders)).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));
		TypedQuery<Long> query = entityManager.createQuery(criteriaQuery);
		return query.getSingleResult();
	}

	/**
	 * @param orderListFilterDto
	 * @param criteriaBuilder
	 * @param orders
	 * @param predicates
	 */
	private void addConditions(final OrderListFilterDto orderListFilterDto, final CriteriaBuilder criteriaBuilder, final Root<Orders> orders,
			final List<Predicate> predicates) {
		if (orderListFilterDto.getVendorId() != null) {
			Join<Orders, Vendor> store = orders.join(VENDOR_PARAM, JoinType.INNER);
			predicates.add(criteriaBuilder.equal(store.get("id"), orderListFilterDto.getVendorId()));
		}
		if (orderListFilterDto.getDeliveryBoyId() != null) {
			Join<Orders, DeliveryBoy> deliveryBoy = orders.join(DELIVERY_BOY, JoinType.INNER);
			predicates.add(criteriaBuilder.equal(deliveryBoy.get("id"), orderListFilterDto.getDeliveryBoyId()));
		}

		if (orderListFilterDto.getReplacementDeliveryBoyId() != null) {
			Join<Orders, DeliveryBoy> replacementDeliveryBoy = orders.join(REPLACEMENT_DELIVERY_BOY, JoinType.INNER);
			predicates.add(criteriaBuilder.equal(replacementDeliveryBoy.get("id"), orderListFilterDto.getReplacementDeliveryBoyId()));
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(orderListFilterDto.getPaymentMode())) {
			predicates.add(criteriaBuilder.equal(orders.get(PAYMENT_MODE), orderListFilterDto.getPaymentMode()));
		}
		if (orderListFilterDto.getCustomerId() != null) {
			Join<Orders, Customer> customer = orders.join(CUSTOMER, JoinType.INNER);
			predicates.add(criteriaBuilder.equal(customer.get("id"), orderListFilterDto.getCustomerId()));
		}
		if (orderListFilterDto.getSearchKeyword() != null) {
			Join<Orders, Customer> customer = orders.join(CUSTOMER, JoinType.INNER);
			Predicate predicateForFirstname = criteriaBuilder.like(criteriaBuilder.lower(customer.get("firstName")),
					"%" + orderListFilterDto.getSearchKeyword().toLowerCase() + "%");
			Predicate predicateForLastname = criteriaBuilder.like(criteriaBuilder.lower(customer.get("lastName")),
					"%" + orderListFilterDto.getSearchKeyword().toLowerCase() + "%");
			Predicate predicateForOrderId = criteriaBuilder.like(orders.get("id").as(String.class),
					"%" + orderListFilterDto.getSearchKeyword().toLowerCase() + "%");
			Predicate predicateForSearch = criteriaBuilder.or(predicateForFirstname, predicateForLastname, predicateForOrderId);
			predicates.add(predicateForSearch);
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(orderListFilterDto.getOrderStatus())) {
			predicates.add(orders.get(ORDER_STATUS).in(orderListFilterDto.getOrderStatus()));
		}
		if (orderListFilterDto.getOrderDate() != null) {
			predicates.add(criteriaBuilder.equal(orders.get("createdAt").as(Date.class), orderListFilterDto.getOrderDate()));
		}

		if (orderListFilterDto.getDeliveryDate() != null) {
			predicates.add(criteriaBuilder.equal(orders.get(DELIVERY_DATE).as(Date.class), orderListFilterDto.getDeliveryDate()));
		}
		if (orderListFilterDto.getIsForPaymentTransaction() != null && orderListFilterDto.getIsForPaymentTransaction().booleanValue()) {
			/***
			 * get orders which payment is online or offline and delivered
			 */
			if (!CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(orderListFilterDto.getPaymentMode()) && orderListFilterDto.getPaymentDate() == null) {
				predicates.add(criteriaBuilder.or(criteriaBuilder.equal(orders.get(PAYMENT_MODE), PaymentMode.ONLINE.name()),
						criteriaBuilder.isNotNull(orders.get(DELIVERY_DATE))));
			}
			/**
			 * get all offline orders
			 */
			else if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(orderListFilterDto.getPaymentMode())
					&& PaymentMode.COD.name().equals(orderListFilterDto.getPaymentMode()) && orderListFilterDto.getPaymentDate() == null) {
				predicates.add(criteriaBuilder.isNotNull(orders.get(DELIVERY_DATE)));
			}
			/**
			 * get online order for date
			 */
			else if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(orderListFilterDto.getPaymentMode())
					&& PaymentMode.ONLINE.name().equals(orderListFilterDto.getPaymentMode()) && orderListFilterDto.getPaymentDate() != null) {
				predicates.add(criteriaBuilder.equal(orders.get("createdAt").as(Date.class), orderListFilterDto.getPaymentDate()));
			}
			/**
			 * get offline order for date
			 */
			else if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(orderListFilterDto.getPaymentMode())
					&& PaymentMode.COD.name().equals(orderListFilterDto.getPaymentMode()) && orderListFilterDto.getPaymentDate() != null) {
				predicates.add(criteriaBuilder.equal(orders.get(DELIVERY_DATE).as(Date.class), orderListFilterDto.getPaymentDate()));
			}
		}
	}

}
