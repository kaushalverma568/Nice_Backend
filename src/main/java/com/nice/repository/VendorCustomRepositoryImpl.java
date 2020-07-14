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
import javax.persistence.criteria.Expression;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.springframework.stereotype.Repository;

import com.nice.constant.DeliveryType;
import com.nice.constant.VendorStatus;
import com.nice.dto.VendorFilterDTO;
import com.nice.dto.VendorListFilterDTO;
import com.nice.model.BusinessCategory;
import com.nice.model.City;
import com.nice.model.Country;
import com.nice.model.Pincode;
import com.nice.model.Vendor;
import com.nice.model.VendorCuisine;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab Pvt. Ltd.
 * @date   : 29-06-2020
 */
@Repository(value = "vendorCustomRepository")
public class VendorCustomRepositoryImpl implements VendorCustomRepository {

	/**
	 *
	 */
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
		 * Create and add a query root corresponding to the vendor.It is similar to the FROM clause in a JPQL query.
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
		 * Reducing multiple queries into single queries using graph </br>
		 * It allows defining a template by grouping the related persistence fields which we want to retrieve and lets us choose
		 * the graph type at runtime.
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

		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendorFilterDTO.getSearchKeyword())) {
			Expression<String> concatOfFirstName = criteriaBuilder.concat(criteriaBuilder.lower(vendor.get("firstName")), " ");
			Expression<String> fullName = criteriaBuilder.concat(concatOfFirstName, criteriaBuilder.lower(vendor.get("lastName")));
			Predicate predicateForStoreName = criteriaBuilder.like(criteriaBuilder.lower(vendor.get("storeName")),
					"%" + vendorFilterDTO.getSearchKeyword().toLowerCase() + "%");
			predicates.add(
					criteriaBuilder.or(criteriaBuilder.like(fullName, "%" + vendorFilterDTO.getSearchKeyword().toLowerCase() + "%"), predicateForStoreName));
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
		 * Create and add a query root corresponding to the vendor.It is similar to the FROM clause in a JPQL query.
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

	@Override
	public List<Vendor> getVendorListForCustomerBasedOnParams(final Integer startIndex, final Integer pageSize, final VendorListFilterDTO vendorListFilterDTO) {
		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		/**
		 * Create Criteria query object whichever object you want to return.
		 */
		CriteriaQuery<Vendor> criteriaQuery = criteriaBuilder.createQuery(Vendor.class);
		/**
		 * Create and add a query root corresponding to the vendor.It is similar to the FROM clause in a JPQL query.
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
		addConditionsForCustomerApp(vendorListFilterDTO, criteriaBuilder, vendor, businessCategory, predicates);
		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(vendor).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));

		/**
		 * Reducing multiple queries into single queries using graph </br>
		 * It allows defining a template by grouping the related persistence fields which we want to retrieve and lets us choose
		 * the graph type at runtime.
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

	private void addConditionsForCustomerApp(final VendorListFilterDTO vendorListFilterDTO, final CriteriaBuilder criteriaBuilder, final Root<Vendor> vendor,
			final Join<Vendor, BusinessCategory> businessCategory, final List<Predicate> predicates) {
		predicates.add(criteriaBuilder.equal(vendor.get("active"), true));
		predicates.add(criteriaBuilder.equal(vendor.get("isOrderServiceEnable"), true));
		predicates.add(criteriaBuilder.equal(vendor.get("status"), VendorStatus.ACTIVE.getStatusValue()));
		if (vendorListFilterDTO.getOpeningHours() != null) {
			predicates.add(criteriaBuilder.lessThanOrEqualTo(vendor.get("openingHoursFrom"), vendorListFilterDTO.getOpeningHours()));
			predicates.add(criteriaBuilder.greaterThanOrEqualTo(vendor.get("openingHoursTo"), vendorListFilterDTO.getOpeningHours()));
		}
		predicates.add(criteriaBuilder.equal(businessCategory.get("id"), vendorListFilterDTO.getBusinessCategoryId()));
		if (vendorListFilterDTO.getDeliveryType() != null) {
			Predicate predicate = criteriaBuilder.or(criteriaBuilder.equal(vendor.get("deliveryType"), vendorListFilterDTO.getDeliveryType()),
					criteriaBuilder.equal(vendor.get("deliveryType"), DeliveryType.BOTH.getStatusValue()));
			predicates.add(predicate);
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(vendorListFilterDTO.getSearchKeyword())) {
			Expression<String> storeName = criteriaBuilder.lower(vendor.get("storeName"));
			predicates.add(criteriaBuilder.like(storeName, "%" + vendorListFilterDTO.getSearchKeyword().toLowerCase() + "%"));
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(vendorListFilterDTO.getCuisineIds())) {
			Join<Vendor, VendorCuisine> vendorCuisine = vendor.join("vendorCuisine", JoinType.INNER);
			predicates.add(vendorCuisine.get("cuisine").in(vendorListFilterDTO.getCuisineIds()));
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(vendorListFilterDTO.getVendorIds())) {
			predicates.add(vendor.get("id").in(vendorListFilterDTO.getVendorIds()));
		}
	}

}