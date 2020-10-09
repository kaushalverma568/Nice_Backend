/**
 *
 */
package com.nice.repository;

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

import com.nice.model.Area;
import com.nice.model.City;
import com.nice.model.Country;
import com.nice.model.Customer;
import com.nice.model.CustomerAddress;
import com.nice.model.State;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 25-Jun-2020
 */
@Repository(value = "customerAddressCustomRepository")
public class CustomerAddressCustomRepositoryImpl implements CustomerAddressCustomRepository {

	@PersistenceContext
	private EntityManager entityManager;

	@Override
	public List<CustomerAddress> getCustomerAddressListBasedOnParams(final Boolean activeRecords, final Long customerId, final Long countryId,
			final Long stateId, final Long cityId, final Long areaId, final Integer startIndex, final Integer pageSize) {

		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		/**
		 * Create Criteria query object whichever object you want to return.
		 */
		CriteriaQuery<CustomerAddress> criteriaQuery = criteriaBuilder.createQuery(CustomerAddress.class);
		/**
		 * Create and add a query root corresponding to the CustomerAddress .It is similar to the FROM clause in a JPQL query.
		 */
		Root<CustomerAddress> customerAddressRoot = criteriaQuery.from(CustomerAddress.class);

		/**
		 * Inner Join to the other tables we can filter on like country & Create the standard restrictions (i.e. the standard
		 * where clauses).
		 */
		List<Predicate> predicates = new ArrayList<>();
		if (activeRecords != null) {
			predicates.add(criteriaBuilder.equal(customerAddressRoot.get("active"), activeRecords));
		}

		if (customerId != null) {
			Join<CustomerAddress, Customer> customerRoot = customerAddressRoot.join("customer", JoinType.INNER);
			predicates.add(criteriaBuilder.equal(customerRoot.get("id"), customerId));
		}

		if (countryId != null) {
			Join<CustomerAddress, Country> countryRoot = customerAddressRoot.join("country", JoinType.INNER);
			predicates.add(criteriaBuilder.equal(countryRoot.get("id"), countryId));
		}

		if (stateId != null) {
			Join<CustomerAddress, State> stateRoot = customerAddressRoot.join("state", JoinType.INNER);
			predicates.add(criteriaBuilder.equal(stateRoot.get("id"), countryId));
		}

		if (cityId != null) {
			Join<CustomerAddress, City> cityRoot = customerAddressRoot.join("city", JoinType.INNER);
			predicates.add(criteriaBuilder.equal(cityRoot.get("id"), cityId));
		}

		if (areaId != null) {
			Join<CustomerAddress, Area> areaRoot = customerAddressRoot.join("area", JoinType.INNER);
			predicates.add(criteriaBuilder.equal(areaRoot.get("id"), areaId));
		}
		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(customerAddressRoot).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));

		/**
		 * Reducing multiple queries into single queries using graph </br>
		 * It allows defining a template by grouping the related persistence fields which we want to retrieve and lets us choose
		 * the graph type at runtime.
		 */
		EntityGraph<CustomerAddress> fetchGraph = entityManager.createEntityGraph(CustomerAddress.class);
		fetchGraph.addSubgraph("country");
		TypedQuery<CustomerAddress> query = entityManager.createQuery(criteriaQuery).setHint("javax.persistence.loadgraph", fetchGraph);
		if (startIndex != null && pageSize != null) {
			query.setFirstResult(startIndex);
			query.setMaxResults(pageSize);
		}

		return query.getResultList();
	}

}
