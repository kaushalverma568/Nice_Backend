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

import com.nice.model.City;
import com.nice.model.Pincode;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 23-Jun-2020
 */
@Repository
public class PincodeCustomRepositoryImpl implements PincodeCustomRepository {

	/**
	 *
	 */
	private static final String CITY_PARAM = "city";

	@PersistenceContext
	private EntityManager entityManager;

	@Override
	public List<Pincode> getPincodeListBasedOnParams(final Integer startIndex, final Integer pageSize, final Boolean activeRecords, final Long cityId,
			final String searchKeyword) {
		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		/**
		 * Create Criteria query object whichever object you want to return.
		 */
		CriteriaQuery<Pincode> criteriaQuery = criteriaBuilder.createQuery(Pincode.class);
		/**
		 * Create and add a query root corresponding to the pincode.It is similar to the FROM clause in a JPQL query.
		 */
		Root<Pincode> pincode = criteriaQuery.from(Pincode.class);
		/**
		 * Inner Join to the other tables we can filter on like city.
		 */
		Join<Pincode, City> city = pincode.join(CITY_PARAM, JoinType.INNER);

		/**
		 * Create the standard restrictions (i.e. the standard where clauses).
		 */
		List<Predicate> predicates = new ArrayList<>();

		if (activeRecords != null) {
			predicates.add(criteriaBuilder.equal(pincode.get("active"), activeRecords));
		}

		if (cityId != null) {
			predicates.add(criteriaBuilder.equal(city.get("id"), cityId));
		}

		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(searchKeyword)) {
			predicates.add(criteriaBuilder.like(criteriaBuilder.lower(pincode.get("codeValue")), "%" + searchKeyword.toLowerCase() + "%"));
		}

		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(pincode).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));

		/**
		 * Reducing multiple queries into single queries using graph </br>
		 * It allows defining a template by grouping the related persistence fields which we want to retrieve and lets us choose
		 * the graph type at runtime.
		 */
		EntityGraph<Pincode> fetchGraph = entityManager.createEntityGraph(Pincode.class);
		fetchGraph.addSubgraph(CITY_PARAM);
		TypedQuery<Pincode> query = entityManager.createQuery(criteriaQuery).setHint("javax.persistence.loadgraph", fetchGraph);
		if (startIndex != null && pageSize != null) {
			query.setFirstResult(startIndex);
			query.setMaxResults(pageSize);
		}
		return query.getResultList();
	}

	@Override
	public Long getPincodeCountBasedOnParams(final Boolean activeRecords, final Long cityId, final String searchKeyword) {
		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		/**
		 * Create Criteria query object whichever object you want to return.
		 */
		CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
		/**
		 * Create and add a query root corresponding to the pincode.It is similar to the FROM clause in a JPQL query.
		 */
		Root<Pincode> pincode = criteriaQuery.from(Pincode.class);
		/**
		 * Inner Join to the other tables we can filter on like city.
		 */
		Join<Pincode, City> city = pincode.join(CITY_PARAM, JoinType.INNER);

		/**
		 * Create the standard restrictions (i.e. the standard where clauses).
		 */
		List<Predicate> predicates = new ArrayList<>();

		if (activeRecords != null) {
			predicates.add(criteriaBuilder.equal(pincode.get("active"), activeRecords));
		}

		if (cityId != null) {
			predicates.add(criteriaBuilder.equal(city.get("id"), cityId));
		}

		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(searchKeyword)) {
			predicates.add(criteriaBuilder.like(criteriaBuilder.lower(pincode.get("codeValue")), "%" + searchKeyword.toLowerCase() + "%"));
		}

		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(criteriaBuilder.count(pincode)).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));

		TypedQuery<Long> query = entityManager.createQuery(criteriaQuery);
		return query.getSingleResult();
	}
}
