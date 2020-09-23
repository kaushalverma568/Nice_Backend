/**
 *
 */
package com.nice.repository;

import java.math.BigInteger;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;

import org.springframework.context.i18n.LocaleContextHolder;

import com.nice.dto.DeliveryBoyPayoutDTO;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 29-Jun-2020
 */
public class PaymentDetailsCustomRepositoryImpl implements PaymentDetailsCustomRepository {

	@PersistenceContext
	private EntityManager entityManager;

	/**
	 * @param  registeredOn
	 * @param  deliveryBoyId
	 * @param  searchId
	 * @param  productParamRequestDTO
	 * @param  sqlQuery
	 * @param  paramMap
	 * @return
	 */
	private StringBuilder addConditions(final Long searchId, final Long deliveryBoyId, final Date registeredOn, final StringBuilder sqlQuery,
			final Map<String, Object> paramMap) {
		if (deliveryBoyId != null) {
			sqlQuery.append(" and d.id = :deliveryBoyId ");
			paramMap.put("deliveryBoyId", deliveryBoyId);
		}
		if (registeredOn != null) {
			sqlQuery.append(" and d.created_at = :registeredOn ");
			paramMap.put("registeredOn", registeredOn);
		}

		if (searchId != null) {
//			sqlQuery.append(" and (tsk.order_id like CONCAT('%', :searchId, '%') OR ((d.id) like CONCAT('%', :searchId, '%')))");
//			paramMap.put("searchId", searchId);
		}
		return sqlQuery;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<DeliveryBoyPayoutDTO> getDeliveryBoyPayout(final Long searchId, final Long deliveryBoyId, final Date registeredOn, final Integer startIndex,
			final Integer pageSize) {
		Locale locale = LocaleContextHolder.getLocale();
		Map<String, Object> paramMap = new HashMap<>();
		StringBuilder sqlQuery = new StringBuilder("select d.id as delivery_boy_id,");

		if (locale.getLanguage().equals("en")) {
			sqlQuery.append("concat(d.first_name_english,' ',d.last_name_english)");
		} else {
			sqlQuery.append("concat(d.first_name_arabic,' ',d.last_name_arabic)");
		}
		sqlQuery.append(
				" as delivery_boy_name,d.phone_number as delivery_boy_phone_number,d.created_at as registered_on ,sum(que.cart_orders) as cart_orders,sum(que.replace_orders) as replace_orders,sum(que.return_orders) as return_orders,sum(cart_orders+replace_orders+return_orders) as total_attened,max(pd.paid_on) as last_payment_on,sum(pd.payment_amount) as total_paid from delivery_boy d left join (\r\n"
						+ "select dboy.id as delivery_boy_id, count(t.id)as cart_orders, 0 as replace_orders, 0 as return_orders  from delivery_boy dboy  join task t on dboy.id=t.delivery_boy_id \r\n"
						+ "where t.task_type='Delivery' group by (dboy.id) union\r\n"
						+ "select dboy.id as delivery_boy_id, 0 as cart_orders, count(t.id) as replace_orders, 0 as return_orders  from delivery_boy dboy  join task t on dboy.id=t.delivery_boy_id \r\n"
						+ "where t.task_type='Replacement' group by (dboy.id) union\r\n"
						+ "select dboy.id as delivery_boy_id, 0 as cart_orders, 0 as replace_orders,  count(t.id) as return_orders  from delivery_boy dboy  join task t on dboy.id=t.delivery_boy_id \r\n"
						+ "where t.task_type='Return' group by (dboy.id) )as que\r\n"
						+ "on d.id=que.delivery_boy_id  left join payment_details pd on d.id=pd.delivery_boy_id where 1=1 ");

		addConditions(searchId, deliveryBoyId, registeredOn, sqlQuery, paramMap);

		sqlQuery.append("group by(d.id)");

		sqlQuery.append(" ORDER BY d.id desc ");

		if (startIndex != null && pageSize != null) {
			sqlQuery.append(" offset :startIndex  limit :pageSize ");
			paramMap.put("startIndex", startIndex);
			paramMap.put("pageSize", pageSize);

		}

		Query q = entityManager.createNativeQuery(sqlQuery.toString(), "DeliveryBoyPayout");
		paramMap.entrySet().forEach(p -> q.setParameter(p.getKey(), p.getValue()));
		return q.getResultList();
	}

	@Override
	public Long getDeliveryBoyPayoutCountBasedOnParam(final Long searchId, final Long deliveryBoyId, final Date registeredOn) {
		Map<String, Object> paramMap = new HashMap<>();
		/**
		 * Param For admin/customer/vendor
		 */

		StringBuilder sqlQuery = new StringBuilder("select count(delivery_boy_id)as count1 from (select d.id as delivery_boy_id,");
		Locale locale = LocaleContextHolder.getLocale();

		if (locale.getLanguage().equals("en")) {
			sqlQuery.append("concat(d.first_name_english,' ',d.last_name_english)");
		} else {
			sqlQuery.append("concat(d.first_name_arabic,' ',d.last_name_arabic)");
		}
		sqlQuery.append(
				" as delivery_boy_name,d.phone_number as delivery_boy_phone_number,d.created_at as registered_on ,sum(que.cart_orders) as cart_orders,sum(que.replace_orders) as replace_orders,sum(que.return_orders) as return_orders,sum(cart_orders+replace_orders+return_orders) as total_attened,max(pd.paid_on) as last_payment_on,sum(pd.payment_amount) as total_paid from delivery_boy d left join (\r\n"
						+ "select dboy.id as delivery_boy_id, count(t.id)as cart_orders, 0 as replace_orders, 0 as return_orders  from delivery_boy dboy  join task t on dboy.id=t.delivery_boy_id \r\n"
						+ "where t.task_type='Delivery' group by (dboy.id) union\r\n"
						+ "select dboy.id as delivery_boy_id, 0 as cart_orders, count(t.id) as replace_orders, 0 as return_orders  from delivery_boy dboy  join task t on dboy.id=t.delivery_boy_id \r\n"
						+ "where t.task_type='Replacement' group by (dboy.id) union\r\n"
						+ "select dboy.id as delivery_boy_id, 0 as cart_orders, 0 as replace_orders,  count(t.id) as return_orders  from delivery_boy dboy  join task t on dboy.id=t.delivery_boy_id \r\n"
						+ "where t.task_type='Return' group by (dboy.id) )as que\r\n"
						+ "on d.id=que.delivery_boy_id  left join payment_details pd on d.id=pd.delivery_boy_id where 1=1 ");

		addConditions(searchId, deliveryBoyId, registeredOn, sqlQuery, paramMap);
		sqlQuery.append(" group by(d.id)) as abc");

		Query q = entityManager.createNativeQuery(sqlQuery.toString());
		paramMap.entrySet().forEach(p -> q.setParameter(p.getKey(), p.getValue()));
		return ((BigInteger) q.getSingleResult()).longValue();
	}
}
