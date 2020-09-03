/**
 *
 */
package com.nice.repository;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Date;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.persistence.EntityGraph;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Repository;

import com.nice.constant.DeliveryType;
import com.nice.constant.VendorStatus;
import com.nice.dto.VendorFilterDTO;
import com.nice.dto.VendorListFilterDTO;
import com.nice.model.BusinessCategory;
import com.nice.model.City;
import com.nice.model.Country;
import com.nice.model.Pincode;
import com.nice.model.SubscriptionPlan;
import com.nice.model.Vendor;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date : 29-06-2020
 */
@Repository(value = "vendorCustomRepository")
public class VendorCustomRepositoryImpl implements VendorCustomRepository {

	/**
	 *
	 */

	private static final Logger LOGGER = LoggerFactory.getLogger(VendorCustomRepositoryImpl.class);
	private static final String BUSINESS_CATEGORY_PARAM = "businessCategory";

	@PersistenceContext
	private EntityManager entityManager;

	@Override
	public List<Vendor> getVendorListBasedOnParams(final Integer startIndex, final Integer pageSize, final VendorFilterDTO vendorFilterDTO) {
		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		/**
		 * Create Criteria query object whichever object you want to return.
		 */
		CriteriaQuery<Vendor> criteriaQuery = criteriaBuilder.createQuery(Vendor.class);
		/**
		 * Create and add a query root corresponding to the vendor.It is similar to the
		 * FROM clause in a JPQL query.
		 */
		Root<Vendor> vendor = criteriaQuery.from(Vendor.class);
		/**
		 * Inner Join to the other tables we can filter on like business category.
		 */
		Join<Vendor, BusinessCategory> businessCategory = vendor.join(BUSINESS_CATEGORY_PARAM, JoinType.INNER);

		/**
		 * Create the standard restrictions (i.e. the standard where clauses).
		 */
		List<Predicate> predicates = new ArrayList<>();

		addConditions(vendorFilterDTO, criteriaBuilder, vendor, businessCategory, predicates);

		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(vendor).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));
		/**
		 * add sorting
		 */
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendorFilterDTO.getSortByDirection())
				&& CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendorFilterDTO.getSortByField())) {
			if (Direction.fromString(vendorFilterDTO.getSortByDirection()).isAscending()) {
				criteriaQuery.orderBy(criteriaBuilder.asc(vendor.get(vendorFilterDTO.getSortByField())));
			} else {
				criteriaQuery.orderBy(criteriaBuilder.desc(vendor.get(vendorFilterDTO.getSortByField())));
			}
		}
		/**
		 * Reducing multiple queries into single queries using graph </br>
		 * It allows defining a template by grouping the related persistence fields
		 * which we want to retrieve and lets us choose the graph type at runtime.
		 */
		EntityGraph<Vendor> fetchGraph = entityManager.createEntityGraph(Vendor.class);
		fetchGraph.addSubgraph(BUSINESS_CATEGORY_PARAM);
		TypedQuery<Vendor> query = entityManager.createQuery(criteriaQuery).setHint("javax.persistence.loadgraph", fetchGraph);
		if (startIndex != null && pageSize != null) {
			query.setFirstResult(startIndex);
			query.setMaxResults(pageSize);
		}
		return query.getResultList();
	}

	/**
	 * @param vendorFilterDTO
	 * @param criteriaBuilder
	 * @param vendor
	 * @param businessCategory
	 * @param predicates
	 */
	private void addConditions(final VendorFilterDTO vendorFilterDTO, final CriteriaBuilder criteriaBuilder, final Root<Vendor> vendor,
			final Join<Vendor, BusinessCategory> businessCategory, final List<Predicate> predicates) {
		if (vendorFilterDTO.getActiveRecords() != null) {
			predicates.add(criteriaBuilder.equal(vendor.get("active"), vendorFilterDTO.getActiveRecords()));
		}

		if (vendorFilterDTO.getBusinessCategoryId() != null) {
			predicates.add(criteriaBuilder.equal(businessCategory.get("id"), vendorFilterDTO.getBusinessCategoryId()));
		}

		if (vendorFilterDTO.getCountryId() != null) {
			Join<Vendor, Country> country = vendor.join("country", JoinType.INNER);
			predicates.add(criteriaBuilder.equal(country.get("id"), vendorFilterDTO.getCountryId()));
		}

		if (vendorFilterDTO.getPincodeId() != null) {
			Join<Vendor, Pincode> pincode = vendor.join("pincode", JoinType.INNER);
			predicates.add(criteriaBuilder.equal(pincode.get("id"), vendorFilterDTO.getPincodeId()));
		}

		if (vendorFilterDTO.getCityId() != null) {
			Join<Vendor, City> city = vendor.join("city", JoinType.INNER);
			predicates.add(criteriaBuilder.equal(city.get("id"), vendorFilterDTO.getCityId()));
		}

		if (vendorFilterDTO.getSubscriptionEndDate() != null) {
			predicates.add(criteriaBuilder.equal(vendor.get("subscriptionPlanEndDate").as(Date.class), vendorFilterDTO.getSubscriptionEndDate()));
		}
		if (vendorFilterDTO.getIsFeatured() != null) {
			predicates.add(criteriaBuilder.equal(vendor.get("isFeatured"), vendorFilterDTO.getIsFeatured()));
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendorFilterDTO.getSearchKeyword())) {
			Expression<String> fullName;
			Predicate predicateForStoreName;
			if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
				Expression<String> concatOfFirstName = criteriaBuilder.concat(criteriaBuilder.lower(vendor.get("firstNameEnglish")), " ");
				fullName = criteriaBuilder.concat(concatOfFirstName, criteriaBuilder.lower(vendor.get("lastNameEnglish")));
				predicateForStoreName = criteriaBuilder.like(criteriaBuilder.lower(vendor.get("storeNameEnglish")),
						"%" + vendorFilterDTO.getSearchKeyword().toLowerCase() + "%");
			} else {
				Expression<String> concatOfFirstName = criteriaBuilder.concat(criteriaBuilder.lower(vendor.get("firstNameArabic")), " ");
				fullName = criteriaBuilder.concat(concatOfFirstName, criteriaBuilder.lower(vendor.get("lastNameArabic")));
				predicateForStoreName = criteriaBuilder.like(criteriaBuilder.lower(vendor.get("storeNameArabic")),
						"%" + vendorFilterDTO.getSearchKeyword().toLowerCase() + "%");
			}
			predicates.add(
					criteriaBuilder.or(criteriaBuilder.like(fullName, "%" + vendorFilterDTO.getSearchKeyword().toLowerCase() + "%"), predicateForStoreName));
		}

		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendorFilterDTO.getStatus())) {
			predicates.add(criteriaBuilder.equal(vendor.get("status"), vendorFilterDTO.getStatus()));
		}

		if (vendorFilterDTO.getSubscriptionPlanId() != null) {
			Join<Vendor, SubscriptionPlan> subscriptionPlan = vendor.join("subscriptionPlan", JoinType.INNER);
			predicates.add(criteriaBuilder.equal(subscriptionPlan.get("id"), vendorFilterDTO.getSubscriptionPlanId()));
		}
	}

	@Override
	public Long getVendorCountBasedOnParams(final VendorFilterDTO vendorFilterDTO) {
		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		/**
		 * Create Criteria query object whichever object you want to return.
		 */
		CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
		/**
		 * Create and add a query root corresponding to the vendor.It is similar to the
		 * FROM clause in a JPQL query.
		 */
		Root<Vendor> vendor = criteriaQuery.from(Vendor.class);
		/**
		 * Inner Join to the other tables we can filter on like businessCategory.
		 */
		Join<Vendor, BusinessCategory> businessCategory = vendor.join(BUSINESS_CATEGORY_PARAM, JoinType.INNER);

		/**
		 * Create the standard restrictions (i.e. the standard where clauses).
		 */
		List<Predicate> predicates = new ArrayList<>();

		addConditions(vendorFilterDTO, criteriaBuilder, vendor, businessCategory, predicates);
		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(criteriaBuilder.count(vendor)).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));

		TypedQuery<Long> query = entityManager.createQuery(criteriaQuery);
		return query.getSingleResult();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<Vendor> getVendorListForCustomerBasedOnParams(final Integer startIndex, final Integer pageSize, final VendorListFilterDTO vendorListFilterDTO) {
		Map<String, Object> paramMap = new HashMap<>();
		StringBuilder sqlQuery = new StringBuilder(
				"SELECT v.id,v.email,v.first_name_english,v.last_name_english,v.store_name_english,v.store_image_name,v.store_detail_image_name,v.featured_image_name,v.latitude,v.longitude,v.rating,v.no_of_rating,v.is_featured,v.accepts,v.phone_number,v.opening_hours_from,v.opening_hours_to,v.delivery_type,v.payment_method,v.minimum_order_amt,v.store_phone_number,,v.first_name_arabic,v.last_name_arabic,v.store_name_arabic, (( 3959 * acos( cos( radians(:customerLatitude) ) * cos( radians(latitude) ) * cos( radians(longitude) "
						+ "- radians(:customerLongitude) ) + sin( radians(:customerLatitude) ) * sin( radians(latitude) ) ) )*1.60934) AS distance "
						+ "FROM vendor v ");
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(vendorListFilterDTO.getCuisineIds())) {
			sqlQuery.append(" left join vendor_cuisine vc on v.id = vc.vendor_id left join cuisine cuisine on vc.cuisine_id = cuisine.id");
		}
		sqlQuery.append(" where v.active = true and v.status = '" + VendorStatus.ACTIVE.getStatusValue() + "' and v.is_order_service_enable = true");
		paramMap.put("customerLatitude", vendorListFilterDTO.getLatitude());
		paramMap.put("customerLongitude", vendorListFilterDTO.getLongitude());

		addConditionsForCustomerApp(vendorListFilterDTO, sqlQuery, paramMap);
		sqlQuery.append(" group by (v.id) ");
		if (vendorListFilterDTO.getIsNameSorting() != null && vendorListFilterDTO.getIsNameSorting().booleanValue()) {
			if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
				sqlQuery.append(" order by v.store_name_english asc");
			} else {
				sqlQuery.append(" order by v.store_name_arabic asc");
			}
		} else {
			sqlQuery.append(" order by distance asc");
		}
		if (startIndex != null && pageSize != null) {
			sqlQuery.append(" offset :startIndex  limit :pageSize ");
			paramMap.put("startIndex", startIndex);
			paramMap.put("pageSize", pageSize);
		}
		Query q = entityManager.createNativeQuery(sqlQuery.toString());
		paramMap.entrySet().forEach(p -> q.setParameter(p.getKey(), p.getValue()));
		Object obj = q.getResultList();
		List<Object[]> responseObjList = (List<Object[]>) obj;
		List<Vendor> vendors = new ArrayList<>();
		for (Object[] responseObj : responseObjList) {
			Vendor vendor = new Vendor();
			vendor.setId(Long.valueOf(responseObj[0].toString()));
			vendor.setEmail(String.valueOf(responseObj[1]));
			vendor.setFirstNameEnglish(String.valueOf(responseObj[2]));
			vendor.setLastNameEnglish(String.valueOf(responseObj[3]));
			vendor.setStoreNameEnglish(String.valueOf(responseObj[4]));
			vendor.setStoreImageName(String.valueOf(responseObj[5]));
			vendor.setStoreDetailImageName(String.valueOf(responseObj[6]));
			vendor.setFeaturedImageName(String.valueOf(responseObj[7]));
			vendor.setLatitude(BigDecimal.valueOf(Double.valueOf(responseObj[8].toString())));
			vendor.setLongitude(BigDecimal.valueOf(Double.valueOf(responseObj[9].toString())));
			if (responseObj[10] != null) {
				vendor.setRating(Double.valueOf(responseObj[10].toString()));
			}
			if (responseObj[11] != null) {
				vendor.setNoOfRating(Long.valueOf(responseObj[11].toString()));
			}
			if (responseObj[12] != null) {
				vendor.setIsFeatured(Boolean.valueOf(responseObj[12].toString()));
			}
			vendor.setAccepts(String.valueOf(responseObj[13]));
			vendor.setPhoneNumber(String.valueOf(responseObj[14]));
			if (responseObj[15] != null) {
				try {
					vendor.setOpeningHoursFrom(new SimpleDateFormat("HH:mm:ss").parse(responseObj[15].toString()));
				} catch (ParseException e) {
					LOGGER.info("error while parsing opening hours from date");
				}
			}
			if (responseObj[16] != null) {
				try {
					vendor.setOpeningHoursTo(new SimpleDateFormat("HH:mm:ss").parse(responseObj[16].toString()));
				} catch (ParseException e) {
					LOGGER.info("error while parsing opening hours to date");
				}
			}
			vendor.setDeliveryType(String.valueOf(responseObj[17]));
			vendor.setPaymentMethod(String.valueOf(responseObj[18]));
			if (responseObj[19] != null) {
				vendor.setMinimumOrderAmt(Double.valueOf(responseObj[19].toString()));
			}
			vendor.setStorePhoneNumber(String.valueOf(responseObj[20]));
			vendor.setFirstNameArabic(String.valueOf(responseObj[21]));
			vendor.setLastNameArabic(String.valueOf(responseObj[22]));
			vendor.setStoreNameArabic(String.valueOf(responseObj[23]));
			if (responseObj[21] != null) {
				vendor.setDistance(Double.valueOf(responseObj[24].toString()));
			}
			vendors.add(vendor);
		}
		return vendors;
	}

	private void addConditionsForCustomerApp(final VendorListFilterDTO vendorListFilterDTO, final StringBuilder sqlQuery, final Map<String, Object> paramMap) {
		if (vendorListFilterDTO.getBusinessCategoryId() != null) {
			sqlQuery.append(" and v.business_category_id = :businessCategoryId ");
			paramMap.put("businessCategoryId", vendorListFilterDTO.getBusinessCategoryId());
		}
		if (vendorListFilterDTO.getOpeningHours() != null) {
			sqlQuery.append(" and v.opening_hours_from < :openingHours and v.opening_hours_to > :openingHours ");
			paramMap.put("openingHours", vendorListFilterDTO.getOpeningHours());
		}
		if (vendorListFilterDTO.getDeliveryType() != null) {
			sqlQuery.append(" and (v.delivery_type = :deliveryType or v.delivery_type = '").append(DeliveryType.BOTH.getStatusValue()).append("')");
			paramMap.put("deliveryType", vendorListFilterDTO.getDeliveryType());
		}
		if (vendorListFilterDTO.getIsFeatured() != null) {
			sqlQuery.append(" and v.is_featured = :isFeatured");
			paramMap.put("isFeatured", vendorListFilterDTO.getIsFeatured());
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendorListFilterDTO.getSearchKeyword())) {
			sqlQuery.append(
					" and ( lower(v.store_name_engish) like CONCAT('%', :searchKeyword, '%')  or  lower(v.store_name_arabic) like CONCAT('%', :searchKeyword, '%) )");
			paramMap.put("searchKeyword", vendorListFilterDTO.getSearchKeyword().toLowerCase());
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(vendorListFilterDTO.getCuisineIds())) {
			sqlQuery.append(" and cuisine.id in (" + vendorListFilterDTO.getCuisineIds().stream().map(String::valueOf).collect(Collectors.joining(",")) + ") ");
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(vendorListFilterDTO.getVendorIds())) {
			sqlQuery.append(" and v.id in (" + vendorListFilterDTO.getVendorIds().stream().map(String::valueOf).collect(Collectors.joining(",")) + ") ");
		}
		if (vendorListFilterDTO.getRatingFrom() != null && vendorListFilterDTO.getRatingTo() != null) {
			sqlQuery.append(" and v.rating between :ratingFrom and :ratingTo");
			paramMap.put("ratingFrom", vendorListFilterDTO.getRatingFrom());
			paramMap.put("ratingTo", vendorListFilterDTO.getRatingTo());
		}
		if (vendorListFilterDTO.getCityId() != null) {
			sqlQuery.append(" and v.city_id = :cityId ");
			paramMap.put("cityId", vendorListFilterDTO.getCityId());
		}
	}

	@Override
	public Long getVendorCountForCustomerBasedOnParams(final VendorListFilterDTO vendorListFilterDTO) {
		Map<String, Object> paramMap = new HashMap<>();
		StringBuilder sqlQuery = new StringBuilder(" select count(*) FROM vendor v ");
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(vendorListFilterDTO.getCuisineIds())) {
			sqlQuery.append(" left join vendor_cuisine vc on v.id = vc.vendor_id left join cuisine cuisine on vc.cuisine_id = cuisine.id");
		}
		sqlQuery.append(" where v.active = true and v.status = '" + VendorStatus.ACTIVE.getStatusValue() + "' and v.is_order_service_enable = true ");
		addConditionsForCustomerApp(vendorListFilterDTO, sqlQuery, paramMap);
		Query q = entityManager.createNativeQuery(sqlQuery.toString());
		paramMap.entrySet().forEach(p -> q.setParameter(p.getKey(), p.getValue()));
		return ((BigInteger) q.getSingleResult()).longValue();
	}
}
