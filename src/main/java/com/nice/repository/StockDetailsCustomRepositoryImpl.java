package com.nice.repository;

import java.math.BigInteger;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;

import org.springframework.stereotype.Repository;

import com.nice.constant.Constant;
import com.nice.dto.StockDetailFilterDTO;
import com.nice.model.StockDetails;
import com.nice.util.CommonUtility;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 17-Feb-2020
 */
@Repository(value = "stockDetailsCustomRepository")
public class StockDetailsCustomRepositoryImpl implements StockDetailsCustomRepository {

	@PersistenceContext
	private EntityManager entityManager;

	private static final String STOCKDETAILSMAPPING = "StockDetailsMapping";

	private static final String VENDOR_ID = "vendorId";

	private static final String SELET_BASE_QUERY = " SELECT SD.* FROM stock_details SD inner join product_variant PV on SD.product_variant_id = PV.id ";

	@SuppressWarnings("unchecked")
	@Override
	public List<StockDetails> getStockDetailsListForSerachString(final Integer startIndex, final Integer pageSize,
			final StockDetailFilterDTO stockDetailFilterDTO) {
		Map<String, Object> paramMap = new HashMap<>();
		StringBuilder sqlQuery = new StringBuilder();
		if (stockDetailFilterDTO.getUomId() != null) {
			sqlQuery.append(SELET_BASE_QUERY
					+ " inner join product P on PV.product_id = P.id inner join uom u on PV.uom_id = u.id where 1=1 and u.id = :uomId ");
			paramMap.put("uomId", stockDetailFilterDTO.getUomId());
		} else {
			sqlQuery.append(SELET_BASE_QUERY + " inner join product p on PV.product_id = P.id  where 1=1  ");
		}
		sqlQuery = addConditions(sqlQuery, stockDetailFilterDTO, paramMap);

		if (startIndex != null && pageSize != null) {
			sqlQuery.append(" offset :startIndex  limit :pageSize");
			paramMap.put("startIndex", startIndex);
			paramMap.put("pageSize", pageSize);
		}

		Query q = entityManager.createNativeQuery(sqlQuery.toString(), STOCKDETAILSMAPPING);
		paramMap.entrySet().forEach(p -> q.setParameter(p.getKey(), p.getValue()));
		return q.getResultList();
	}

	@Override
	public Long getStockDetailsListForSerachStringCount(final StockDetailFilterDTO stockDetailFilterDTO) {

		Map<String, Object> paramMap = new HashMap<>();
		StringBuilder sqlQuery = new StringBuilder();
		if (stockDetailFilterDTO.getUomId() != null) {
			sqlQuery.append(
					" SELECT count(*) FROM stock_details SD inner join product_variant PV on SD.product_variant_id = PV.id "
							+ " inner join product P on PV.product_id = P.id inner join uom u on PV.uom_id = u.id where 1=1 and u.id = :uomId");
			paramMap.put("uomId", stockDetailFilterDTO.getUomId());
		} else {
			sqlQuery.append(
					" SELECT count(*) FROM stock_details SD inner join product_variant PV on SD.product_variant_id = PV.id "
							+ " inner join product P on PV.product_id = P.id  where 1=1  ");
		}
		sqlQuery = addConditions(sqlQuery, stockDetailFilterDTO, paramMap);

		Query q = entityManager.createNativeQuery(sqlQuery.toString());
		paramMap.entrySet().forEach(p -> q.setParameter(p.getKey(), p.getValue()));
		return ((BigInteger) q.getSingleResult()).longValue();
	}

	private StringBuilder addConditions(StringBuilder sqlQuery, StockDetailFilterDTO stockDetailFilterDTO,
			Map<String, Object> paramMap) {
		if (stockDetailFilterDTO.getProductId() != null) {
			sqlQuery.append(" and P.id =  :productId");
			paramMap.put("productId", stockDetailFilterDTO.getProductId());
		}
		if (stockDetailFilterDTO.getLotNo() != null) {
			sqlQuery.append(" and SD.lot_no = :lotNo");
			paramMap.put("lotNo", stockDetailFilterDTO.getLotNo());
		}
		if (stockDetailFilterDTO.getExpiryDate() != null) {
			LocalDate expiryDate = stockDetailFilterDTO.getExpiryDate().toInstant().atZone(ZoneId.systemDefault())
					.toLocalDate();
			sqlQuery.append(" and CAST (SD.expiry_date AS DATE) = :expiryDate");
			paramMap.put("expiryDate", expiryDate);
		}
		if (stockDetailFilterDTO.getCratedDate() != null) {
			LocalDate createdDate = stockDetailFilterDTO.getCratedDate().toInstant().atZone(ZoneId.systemDefault())
					.toLocalDate();
			sqlQuery.append(" and CAST (SD.created_at AS DATE) = :createdDate");
			paramMap.put("createdDate", createdDate);
		}
		if (stockDetailFilterDTO.getProductName() != null) {
			sqlQuery.append(" and P.name  like :productName  ");
			paramMap.put("productName", "%" + stockDetailFilterDTO.getProductName() + "%");
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_STRING.test(stockDetailFilterDTO.getSku())) {
			sqlQuery.append(" and PV.sku  = :sku");
			paramMap.put("sku", stockDetailFilterDTO.getSku());
		}
		if (stockDetailFilterDTO.getVendorId() != null) {
			sqlQuery.append(" and SD.vendor_id  = :vendorId");
			paramMap.put(VENDOR_ID, stockDetailFilterDTO.getVendorId());
		}
		if (stockDetailFilterDTO.getSeachLotAndProductName() != null) {
			sqlQuery.append(
					" and ( P.name like :productNameLotNum  or  CAST(SD.lot_no AS VARCHAR)  like :productNameLotNum)");
			paramMap.put("productNameLotNum", "%" + stockDetailFilterDTO.getSeachLotAndProductName() + "%");
		}
		if (stockDetailFilterDTO.getActive() != null) {
			sqlQuery.append(" and SD.active = :active");
			paramMap.put("active", stockDetailFilterDTO.getActive());
		}
		return sqlQuery;
	}

	@Override
	public Long getLowStockProductDetailsCount(Long vendorId) {
		Map<String, Object> paramMap = new HashMap<>();
		StringBuilder sqlQuery = new StringBuilder();
		sqlQuery.append("select count(*) as count1 from( "
				+ " select distinct(stckDet.product_variant_id) from stock_details stckDet where vendor_ID= :vendorId  "
				+ " group by (stckDet.product_variant_id ) "
				+ " having sum(stckDet.available) <= :lowStockLimit ) as abc ");
		paramMap.put("vendorId", vendorId);
		paramMap.put("lowStockLimit", Constant.LOW_STOCK_LIMIT);
		Query q = entityManager.createNativeQuery(sqlQuery.toString());
		paramMap.entrySet().forEach(p -> q.setParameter(p.getKey(), p.getValue()));
		return ((BigInteger) q.getSingleResult()).longValue();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<BigInteger> getLowStockProductDetails(Long vendorId, Integer startIndex, Integer pageSize) {
		Map<String, Object> paramMap = new HashMap<>();
		StringBuilder sqlQuery = new StringBuilder();
		sqlQuery.append(
				"select distinct(stckDet.product_variant_id) from stock_details stckDet where vendor_ID= :vendorId  "
						+ "group by (stckDet.product_variant_id ) "
						+ "having sum(stckDet.available) <= :lowStockLimit ");
		paramMap.put("vendorId", vendorId);
		paramMap.put("lowStockLimit", Constant.LOW_STOCK_LIMIT);
		if (startIndex != null && pageSize != null) {
			sqlQuery.append(" offset :startIndex  limit :pageSize");
			paramMap.put("startIndex", startIndex);
			paramMap.put("pageSize", pageSize);
		}
		Query q = entityManager.createNativeQuery(sqlQuery.toString());
		paramMap.entrySet().forEach(p -> q.setParameter(p.getKey(), p.getValue()));
		return q.getResultList();
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<StockDetails> getExpiredStockDetails(Long vendorId, Integer startIndex, Integer pageSize) {
		LocalDate now = LocalDate.now();
		LocalDate then = now.plusDays(7);
		Date nowDate = CommonUtility.convertLocalDateToUtilDate(now);
		Date thenDate = CommonUtility.convertLocalDateToUtilDate(then);
		Map<String, Object> paramMap = new HashMap<>();
		StringBuilder sqlQuery = new StringBuilder();
		sqlQuery.append("SELECT SD.* FROM stock_details SD "
				+ "  where 1=1 and SD.vendor_id= :vendorId and SD.expiry_date between :startDate and :endDate ");
		paramMap.put("vendorId", vendorId);
		paramMap.put("startDate", nowDate);
		paramMap.put("endDate", thenDate);
		if (startIndex != null && pageSize != null) {
			sqlQuery.append(" offset :startIndex  limit :pageSize");
			paramMap.put("startIndex", startIndex);
			paramMap.put("pageSize", pageSize);
		}
		Query q = entityManager.createNativeQuery(sqlQuery.toString(), STOCKDETAILSMAPPING);
		paramMap.entrySet().forEach(p -> q.setParameter(p.getKey(), p.getValue()));
		return q.getResultList();
	}

	@Override
	public Long getExpiredStockDetailsCount(Long vendorId) {
		LocalDate now = LocalDate.now();
		LocalDate then = now.plusDays(7);
		Date nowDate = CommonUtility.convertLocalDateToUtilDate(now);
		Date thenDate = CommonUtility.convertLocalDateToUtilDate(then);
		Map<String, Object> paramMap = new HashMap<>();
		StringBuilder sqlQuery = new StringBuilder();
		sqlQuery.append("SELECT count(*) FROM stock_details SD "
				+ "  where 1=1 and SD.vendor_id= :vendorId and SD.expiry_date between :startDate and :endDate ");
		paramMap.put("vendorId", vendorId);
		paramMap.put("startDate", nowDate);
		paramMap.put("endDate", thenDate);
		Query q = entityManager.createNativeQuery(sqlQuery.toString());
		paramMap.entrySet().forEach(p -> q.setParameter(p.getKey(), p.getValue()));
		return ((BigInteger) q.getSingleResult()).longValue();
	}

}