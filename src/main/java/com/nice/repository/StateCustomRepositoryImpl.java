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

import com.nice.model.Country;
import com.nice.model.State;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
@Repository
public class StateCustomRepositoryImpl implements StateCustomRepository {

	/**
	 *
	 */
	private static final String COUNTRY_PARAM = "country";
	/**
	 *
	 */
	private static final String NAME_ARABIC = "nameArabic";

	/**
	 *
	 */
	private static final String NAME_ENGLISH = "nameEnglish";

	@PersistenceContext
	private EntityManager entityManager;

	@Override
	public List<State> getStateList(final Integer startIndex, final Integer pageSize, final Boolean activeRecords, final Long countryId,
			final String searchKeyword) {
		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		/**
		 * Create Criteria query object whichever object you want to return.
		 */
		CriteriaQuery<State> criteriaQuery = criteriaBuilder.createQuery(State.class);
		/**
		 * Create and add a query root corresponding to the state.It is similar to the
		 * FROM clause in a JPQL query.
		 */
		Root<State> state = criteriaQuery.from(State.class);
		/**
		 * Inner Join to the other tables we can filter on like country.
		 */
		Join<State, Country> country = state.join(COUNTRY_PARAM, JoinType.INNER);

		/**
		 * Create the standard restrictions (i.e. the standard where clauses).
		 */
		List<Predicate> predicates = new ArrayList<>();
		if (activeRecords != null) {
			predicates.add(criteriaBuilder.equal(state.get("active"), activeRecords));
		}

		if (countryId != null) {
			predicates.add(criteriaBuilder.equal(country.get("id"), countryId));
		}

		if (searchKeyword != null) {
			Predicate predicateForNameEnglish = criteriaBuilder.like(criteriaBuilder.lower(state.get(NAME_ENGLISH)), "%" + searchKeyword.toLowerCase() + "%");
			Predicate predicateForNameArabic = criteriaBuilder.like(criteriaBuilder.lower(state.get(NAME_ARABIC)), "%" + searchKeyword.toLowerCase() + "%");
			predicates.add(criteriaBuilder.or(predicateForNameEnglish, predicateForNameArabic));
		}

		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(state).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])))
				.orderBy(criteriaBuilder.asc(state.get("name")));

		/**
		 * Reducing multiple queries into single queries using graph </br>
		 * It allows defining a template by grouping the related persistence fields
		 * which we want to retrieve and lets us choose the graph type at runtime.
		 */
		EntityGraph<State> fetchGraph = entityManager.createEntityGraph(State.class);
		fetchGraph.addSubgraph(COUNTRY_PARAM);
		TypedQuery<State> query = entityManager.createQuery(criteriaQuery).setHint("javax.persistence.loadgraph", fetchGraph);
		if (startIndex != null && pageSize != null) {
			query.setFirstResult(startIndex);
			query.setMaxResults(pageSize);
		}

		return query.getResultList();
	}

	@Override
	public Long getStateCountBasedonParams(final Boolean activeRecords, final Long countryId, final String searchKeyword) {
		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		/**
		 * Create Criteria query object whichever object you want to return.
		 */
		CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
		/**
		 * Create and add a query root corresponding to the state.It is similar to the
		 * FROM clause in a JPQL query.
		 */
		Root<State> state = criteriaQuery.from(State.class);
		/**
		 * Inner Join to the other tables we can filter on like country.
		 */
		Join<State, Country> country = state.join(COUNTRY_PARAM, JoinType.INNER);

		/**
		 * Create the standard restrictions (i.e. the standard where clauses).
		 */
		List<Predicate> predicates = new ArrayList<>();
		if (activeRecords != null) {
			predicates.add(criteriaBuilder.equal(state.get("active"), activeRecords));
		}

		if (countryId != null) {
			predicates.add(criteriaBuilder.equal(country.get("id"), countryId));
		}

		if (searchKeyword != null) {
			Predicate predicateForNameEnglish = criteriaBuilder.like(criteriaBuilder.lower(state.get(NAME_ENGLISH)), "%" + searchKeyword.toLowerCase() + "%");
			Predicate predicateForNameArabic = criteriaBuilder.like(criteriaBuilder.lower(state.get(NAME_ARABIC)), "%" + searchKeyword.toLowerCase() + "%");
			predicates.add(criteriaBuilder.or(predicateForNameEnglish, predicateForNameArabic));
		}

		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(criteriaBuilder.count(state)).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));

		TypedQuery<Long> query = entityManager.createQuery(criteriaQuery);
		return query.getSingleResult();
	}

}
