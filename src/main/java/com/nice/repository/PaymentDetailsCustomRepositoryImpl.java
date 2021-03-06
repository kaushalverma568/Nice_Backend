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
import com.nice.dto.VendorPayoutDTO;

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
	private StringBuilder addDeliveryBoyPayoutConditions(final Long deliveryBoyId, final Date registeredOn, final StringBuilder sqlQuery,
			final Map<String, Object> paramMap) {
		if (deliveryBoyId != null) {
			sqlQuery.append(" and d.id = :deliveryBoyId ");
			paramMap.put("deliveryBoyId", deliveryBoyId);
		}
		if (registeredOn != null) {
			sqlQuery.append(" and CAST (d.created_at AS DATE) = :registeredOn ");
			paramMap.put("registeredOn", registeredOn);
		}
		return sqlQuery;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<DeliveryBoyPayoutDTO> getDeliveryBoyPayout(final Long deliveryBoyId, final Date registeredOn, final Integer startIndex,
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
				" as delivery_boy_name,d.phone_number as delivery_boy_phone_number,d.created_at as registered_on ,sum(que.cart_orders) as cart_orders,sum(que.replace_orders) as replace_orders,sum(que.return_orders) as return_orders,sum(cart_orders+replace_orders+return_orders) as total_attened,max(que.paid_on) as last_payment_on,sum(que.payment_amount) as total_paid from delivery_boy d join (\r\n"
						+ "select dboy.id as delivery_boy_id, count(t.id)as cart_orders, \r\n"
						+ "0 as replace_orders, 0 as return_orders, NULL as paid_on, 0 as payment_amount\r\n" + "from delivery_boy dboy \r\n"
						+ "join task t on dboy.id=t.delivery_boy_id \r\n" + " where t.task_type='Delivery' \r\n"
						+ "and t.status in('Delivered','Cancelled') group by dboy.id union\r\n" + "\r\n"

						+ "select pd.delivery_boy_id as delivery_boy_id, 0 as cart_orders, \r\n"
						+ "0 as replace_orders, 0 as return_orders, max(pd.paid_on) as paid_on,sum(pd.payment_amount) from payment_details pd \r\n"
						+ "where pd.id in (select distinct delivery_boy_payment_details_id from task where delivery_boy_id=pd.delivery_boy_id) group by pd.delivery_boy_id union\r\n"
						+ "\r\n"

						+ "select dboy.id as delivery_boy_id, 0 as cart_orders, \r\n"
						+ "count(t.id) as replace_orders, 0 as return_orders,NULL as paid_on, 0 as payment_amount\r\n" + "from delivery_boy dboy \r\n"
						+ "join task t on dboy.id=t.delivery_boy_id left join payment_details pd\r\n"
						+ "on t.delivery_boy_payment_details_id =pd.id where t.task_type='Replacement' \r\n"
						+ "and t.status in('Delivered','Cancelled') group by dboy.id union\r\n" + "\r\n"

						+ "select pd.delivery_boy_id as delivery_boy_id, 0 as cart_orders, \r\n"
						+ "0 as replace_orders, 0 as return_orders, max(pd.paid_on) as paid_on,sum(pd.payment_amount) from payment_details pd \r\n"
						+ "where pd.id in (select distinct delivery_boy_payment_details_id from task where delivery_boy_id=pd.delivery_boy_id) group by pd.delivery_boy_id union\r\n"
						+ "\r\n"

						+ "select dboy.id as delivery_boy_id, 0 as cart_orders, \r\n"
						+ "0 as replace_orders, count(t.id) as return_orders, NULL as paid_on, 0 as payment_amount\r\n" + "from delivery_boy dboy \r\n"
						+ "join task t on dboy.id=t.delivery_boy_id left join payment_details pd\r\n"
						+ "on t.delivery_boy_payment_details_id =pd.id where t.task_type='Return' \r\n"
						+ "and t.status in('Delivered','Cancelled') group by dboy.id union\r\n" + "\r\n"

						+ "select pd.delivery_boy_id as delivery_boy_id, 0 as cart_orders, \r\n"
						+ "0 as replace_orders, 0 as return_orders, max(pd.paid_on) as paid_on,sum(pd.payment_amount) from payment_details pd \r\n"
						+ "where pd.id in (select distinct delivery_boy_payment_details_id from task where delivery_boy_id=pd.delivery_boy_id) group by pd.delivery_boy_id \r\n"
						+ "\r\n" + " )as que\r\n" + "on d.id=que.delivery_boy_id where 1=1 ");

		addDeliveryBoyPayoutConditions(deliveryBoyId, registeredOn, sqlQuery, paramMap);

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
	public Long getDeliveryBoyPayoutCountBasedOnParam(final Long deliveryBoyId, final Date registeredOn) {
		Map<String, Object> paramMap = new HashMap<>();

		StringBuilder sqlQuery = new StringBuilder("select count(delivery_boy_id)as count1 from (select d.id as delivery_boy_id,");
		Locale locale = LocaleContextHolder.getLocale();

		if (locale.getLanguage().equals("en")) {
			sqlQuery.append("concat(d.first_name_english,' ',d.last_name_english)");
		} else {
			sqlQuery.append("concat(d.first_name_arabic,' ',d.last_name_arabic)");
		}
		sqlQuery.append(
				" as delivery_boy_name,d.phone_number as delivery_boy_phone_number,d.created_at as registered_on ,sum(que.cart_orders) as cart_orders,sum(que.replace_orders) as replace_orders,sum(que.return_orders) as return_orders,sum(cart_orders+replace_orders+return_orders) as total_attened,max(que.paid_on) as last_payment_on,sum(que.payment_amount) as total_paid from delivery_boy d join (\r\n"
						+ "select dboy.id as delivery_boy_id, count(t.id)as cart_orders, \r\n"
						+ "0 as replace_orders, 0 as return_orders, NULL as paid_on, 0 as payment_amount\r\n" + "from delivery_boy dboy \r\n"
						+ "join task t on dboy.id=t.delivery_boy_id \r\n" + " where t.task_type='Delivery' \r\n"
						+ "and t.status in('Delivered','Cancelled') group by dboy.id union\r\n" + "\r\n"

						+ "select pd.delivery_boy_id as delivery_boy_id, 0 as cart_orders, \r\n"
						+ "0 as replace_orders, 0 as return_orders, max(pd.paid_on) as paid_on,sum(pd.payment_amount) from payment_details pd \r\n"
						+ "where pd.id in (select distinct delivery_boy_payment_details_id from task where delivery_boy_id=pd.delivery_boy_id) group by pd.delivery_boy_id union\r\n"
						+ "\r\n"

						+ "select dboy.id as delivery_boy_id, 0 as cart_orders, \r\n"
						+ "count(t.id) as replace_orders, 0 as return_orders,NULL as paid_on, 0 as payment_amount\r\n" + "from delivery_boy dboy \r\n"
						+ "join task t on dboy.id=t.delivery_boy_id left join payment_details pd\r\n"
						+ "on t.delivery_boy_payment_details_id =pd.id where t.task_type='Replacement' \r\n"
						+ "and t.status in('Delivered','Cancelled') group by dboy.id union\r\n" + "\r\n"

						+ "select pd.delivery_boy_id as delivery_boy_id, 0 as cart_orders, \r\n"
						+ "0 as replace_orders, 0 as return_orders, max(pd.paid_on) as paid_on,sum(pd.payment_amount) from payment_details pd \r\n"
						+ "where pd.id in (select distinct delivery_boy_payment_details_id from task where delivery_boy_id=pd.delivery_boy_id) group by pd.delivery_boy_id union\r\n"
						+ "\r\n"

						+ "select dboy.id as delivery_boy_id, 0 as cart_orders, \r\n"
						+ "0 as replace_orders, count(t.id) as return_orders, NULL as paid_on, 0 as payment_amount\r\n" + "from delivery_boy dboy \r\n"
						+ "join task t on dboy.id=t.delivery_boy_id left join payment_details pd\r\n"
						+ "on t.delivery_boy_payment_details_id =pd.id where t.task_type='Return' \r\n"
						+ "and t.status in('Delivered','Cancelled') group by dboy.id union\r\n" + "\r\n"

						+ "select pd.delivery_boy_id as delivery_boy_id, 0 as cart_orders, \r\n"
						+ "0 as replace_orders, 0 as return_orders, max(pd.paid_on) as paid_on,sum(pd.payment_amount) from payment_details pd \r\n"
						+ "where pd.id in (select distinct delivery_boy_payment_details_id from task where delivery_boy_id=pd.delivery_boy_id) group by pd.delivery_boy_id \r\n"
						+ "\r\n" + " )as que\r\n" + "on d.id=que.delivery_boy_id where 1=1 ");

		addDeliveryBoyPayoutConditions(deliveryBoyId, registeredOn, sqlQuery, paramMap);
		sqlQuery.append(" group by(d.id)) as abc");

		Query q = entityManager.createNativeQuery(sqlQuery.toString());
		paramMap.entrySet().forEach(p -> q.setParameter(p.getKey(), p.getValue()));
		return ((BigInteger) q.getSingleResult()).longValue();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<VendorPayoutDTO> getVendorPayout(final Long vendorId, final Long businessCategoryId, final Integer startIndex, final Integer pageSize) {
		Locale locale = LocaleContextHolder.getLocale();
		Map<String, Object> paramMap = new HashMap<>();
		StringBuilder sqlQuery = new StringBuilder("select v.id as vendor_id,");
		if (locale.getLanguage().equals("en")) {
			sqlQuery.append(
					"concat(v.first_name_english,' ',v.last_name_english) as vendor_name,v.store_name_english as store_name,bc.name_english as business_category_name,");
		} else {
			sqlQuery.append(
					"concat(v.first_name_arabic,' ',v.last_name_arabic) as vendor_name,v.store_name_arabic as store_name,bc.name_arabic as business_category_name,");
		}
		sqlQuery.append(
				"v.phone_number as vendor_phone_number,v.store_phone_number as store_phone_number,v.created_at as registered_on ,bc.id as business_category_id,sum(que.cart_orders) as cart_orders,sum(que.replace_orders) as replace_orders,sum(que.return_orders) as return_orders,sum(cart_orders+replace_orders+return_orders) as total_attened,max(que.paid_on) as last_payment_on,sum(que.payment_amount) as total_paid from vendor v join (\r\n"
						+ "select v.id as vendor_id, count(t.id)as cart_orders, \r\n"
						+ "0 as replace_orders, 0 as return_orders, NULL as paid_on, 0 as payment_amount\r\n" + "from vendor v \r\n"
						+ "join task t on v.id=t.vendor_id left join payment_details pd\r\n"
						+ "on t.vendor_payment_details_id =pd.id where t.task_type='Delivery' \r\n"
						+ "and t.status in('Delivered','Cancelled') group by v.id union\r\n" + "\r\n"

						+ "select pd.vendor_id as vendor_id, 0 as cart_orders, \r\n"
						+ "0 as replace_orders, 0 as return_orders, max(pd.paid_on) as paid_on,sum(pd.payment_amount) from payment_details pd \r\n"
						+ "where pd.id in (select distinct vendor_payment_details_id from task where vendor_id=pd.vendor_id) group by pd.vendor_id union\r\n"
						+ "\r\n"

						+ "select v.id as vendor_id, 0 as cart_orders, \r\n"
						+ "count(t.id) as replace_orders, 0 as return_orders, NULL as paid_on, 0 as payment_amount\r\n" + "from vendor v \r\n"
						+ "join task t on v.id=t.vendor_id left join payment_details pd\r\n"
						+ "on t.vendor_payment_details_id =pd.id where t.task_type='Replacement' \r\n"
						+ "and t.status in('Delivered','Cancelled') group by v.id union\r\n" + "\r\n"

						+ "select pd.vendor_id as vendor_id, 0 as cart_orders, \r\n"
						+ "0 as replace_orders, 0 as return_orders, max(pd.paid_on) as paid_on,sum(pd.payment_amount) from payment_details pd \r\n"
						+ "where pd.id in (select distinct vendor_payment_details_id from task where vendor_id=pd.vendor_id) group by pd.vendor_id union\r\n"
						+ "\r\n"

						+ "select v.id as vendor_id, 0 as cart_orders, \r\n"
						+ "0 as replace_orders, count(t.id) as return_orders, NULL as paid_on, 0 as payment_amount\r\n" + "from vendor v\r\n"
						+ "join task t on v.id=t.vendor_id left join payment_details pd\r\n"
						+ "on t.vendor_payment_details_id =pd.id where t.task_type='Return' \r\n"
						+ "and t.status in('Delivered','Cancelled') group by v.id union\r\n" + "\r\n"

						+ "select pd.vendor_id as vendor_id, 0 as cart_orders, \r\n"
						+ "0 as replace_orders, 0 as return_orders, max(pd.paid_on) as paid_on,sum(pd.payment_amount) from payment_details pd \r\n"
						+ "where pd.id in (select distinct vendor_payment_details_id from task where vendor_id=pd.vendor_id) group by pd.vendor_id \r\n"
						+ "\r\n"

						+ ")as que\r\n"
						+ "on v.id=que.vendor_id join business_category bc on bc.id=v.business_category_id where v.profile_completed='true' and 1=1 ");

		addVendorPayoutConditions(vendorId, businessCategoryId, sqlQuery, paramMap);
		sqlQuery.append("group by(v.id,");
		if (locale.getLanguage().equals("en")) {
			sqlQuery.append("bc.name_english,");
		} else {
			sqlQuery.append("bc.name_arabic,");
		}
		sqlQuery.append("bc.id)");
		sqlQuery.append(" ORDER BY v.id desc ");

		if (startIndex != null && pageSize != null) {
			sqlQuery.append(" offset :startIndex  limit :pageSize ");
			paramMap.put("startIndex", startIndex);
			paramMap.put("pageSize", pageSize);
		}

		Query q = entityManager.createNativeQuery(sqlQuery.toString(), "VendorPayout");
		paramMap.entrySet().forEach(p -> q.setParameter(p.getKey(), p.getValue()));
		return q.getResultList();
	}

	@Override
	public Long getVendorPayoutCountBasedOnParam(final Long vendorId, final Long businessCategoryId) {
		Map<String, Object> paramMap = new HashMap<>();

		StringBuilder sqlQuery = new StringBuilder("select count(vendor_id)as count1 from (select v.id as vendor_id,");
		Locale locale = LocaleContextHolder.getLocale();
		if (locale.getLanguage().equals("en")) {
			sqlQuery.append(
					"concat(v.first_name_english,' ',v.last_name_english) as vendor_name,v.store_name_english as store_name,bc.name_english as business_category_name,");
		} else {
			sqlQuery.append(
					"concat(v.first_name_arabic,' ',v.last_name_arabic) as vendor_name,v.store_name_arabic as store_name,bc.name_arabic as business_category_name,");
		}
		sqlQuery.append(
				"v.phone_number as vendor_phone_number,v.store_phone_number as store_phone_number,v.created_at as registered_on ,bc.id as business_category_id,sum(que.cart_orders) as cart_orders,sum(que.replace_orders) as replace_orders,sum(que.return_orders) as return_orders,sum(cart_orders+replace_orders+return_orders) as total_attened,max(que.paid_on) as last_payment_on,sum(que.payment_amount) as total_paid from vendor v join (\r\n"
						+ "select v.id as vendor_id, count(t.id)as cart_orders, \r\n"
						+ "0 as replace_orders, 0 as return_orders, NULL as paid_on, 0 as payment_amount\r\n" + "from vendor v \r\n"
						+ "join task t on v.id=t.vendor_id left join payment_details pd\r\n"
						+ "on t.vendor_payment_details_id =pd.id where t.task_type='Delivery' \r\n"
						+ "and t.status in('Delivered','Cancelled') group by v.id union\r\n" + "\r\n"

						+ "select pd.vendor_id as vendor_id, 0 as cart_orders, \r\n"
						+ "0 as replace_orders, 0 as return_orders, max(pd.paid_on) as paid_on,sum(pd.payment_amount) from payment_details pd \r\n"
						+ "where pd.id in (select distinct vendor_payment_details_id from task where vendor_id=pd.vendor_id) group by pd.vendor_id union\r\n"
						+ "\r\n"

						+ "select v.id as vendor_id, 0 as cart_orders, \r\n"
						+ "count(t.id) as replace_orders, 0 as return_orders, NULL as paid_on, 0 as payment_amount\r\n" + "from vendor v \r\n"
						+ "join task t on v.id=t.vendor_id left join payment_details pd\r\n"
						+ "on t.vendor_payment_details_id =pd.id where t.task_type='Replacement' \r\n"
						+ "and t.status in('Delivered','Cancelled') group by v.id union\r\n" + "\r\n"

						+ "select pd.vendor_id as vendor_id, 0 as cart_orders, \r\n"
						+ "0 as replace_orders, 0 as return_orders, max(pd.paid_on) as paid_on,sum(pd.payment_amount) from payment_details pd \r\n"
						+ "where pd.id in (select distinct vendor_payment_details_id from task where vendor_id=pd.vendor_id) group by pd.vendor_id union\r\n"
						+ "\r\n"

						+ "select v.id as vendor_id, 0 as cart_orders, \r\n"
						+ "0 as replace_orders, count(t.id) as return_orders, NULL as paid_on, 0 as payment_amount\r\n" + "from vendor v\r\n"
						+ "join task t on v.id=t.vendor_id left join payment_details pd\r\n"
						+ "on t.vendor_payment_details_id =pd.id where t.task_type='Return' \r\n"
						+ "and t.status in('Delivered','Cancelled') group by v.id union\r\n" + "\r\n"

						+ "select pd.vendor_id as vendor_id, 0 as cart_orders, \r\n"
						+ "0 as replace_orders, 0 as return_orders, max(pd.paid_on) as paid_on,sum(pd.payment_amount) from payment_details pd \r\n"
						+ "where pd.id in (select distinct vendor_payment_details_id from task where vendor_id=pd.vendor_id) group by pd.vendor_id \r\n"
						+ "\r\n"

						+ ")as que\r\n"
						+ "on v.id=que.vendor_id join business_category bc on bc.id=v.business_category_id where v.profile_completed='true' and 1=1 ");

		addVendorPayoutConditions(vendorId, businessCategoryId, sqlQuery, paramMap);
		sqlQuery.append(" group by(v.id,");
		if (locale.getLanguage().equals("en")) {
			sqlQuery.append("bc.name_english,");
		} else {
			sqlQuery.append("bc.name_arabic,");
		}
		sqlQuery.append("bc.id)) as abc");

		Query q = entityManager.createNativeQuery(sqlQuery.toString());
		paramMap.entrySet().forEach(p -> q.setParameter(p.getKey(), p.getValue()));
		return ((BigInteger) q.getSingleResult()).longValue();
	}

	private StringBuilder addVendorPayoutConditions(final Long vendorId, final Long businessCategoryId, final StringBuilder sqlQuery,
			final Map<String, Object> paramMap) {
		if (vendorId != null) {
			sqlQuery.append(" and v.id = :vendorId ");
			paramMap.put("vendorId", vendorId);
		}
		if (businessCategoryId != null) {
			sqlQuery.append(" and bc.id = :businessCategoryId ");
			paramMap.put("businessCategoryId", businessCategoryId);
		}
		return sqlQuery;
	}

}
