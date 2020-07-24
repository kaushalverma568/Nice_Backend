package com.nice.service;

import java.util.List;

import com.nice.dto.ProductExtrasDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.model.ProductExtras;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 24-Jul-2020
 */
public interface ProductExtrasService {

	/**
	 * @param  productExtrasDto
	 * @return
	 * @throws NotFoundException
	 */
	boolean isExists(ProductExtrasDTO productExtrasDto) throws NotFoundException;

	/**
	 * @param  productExtrasDto
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Long addProductExtras(ProductExtrasDTO productExtrasDto) throws NotFoundException, ValidationException;

	/**
	 * @param  productExtrasDTO
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	Long updateProductExtras(ProductExtrasDTO productExtrasDTO) throws NotFoundException, ValidationException;

	/**
	 * @param  productExtrasId
	 * @return
	 * @throws NotFoundException
	 */
	ProductExtrasDTO getProductExtras(Long productExtrasId) throws NotFoundException;

	/**
	 * @param  productExtrasId
	 * @param  active
	 * @throws ValidationException
	 * @throws NotFoundException
	 */
	void changeStatus(Long productExtrasId, Boolean active) throws ValidationException, NotFoundException;

	/**
	 * @param  productExtrasId
	 * @return
	 * @throws NotFoundException
	 */
	ProductExtras getProductExtrasDetail(Long productExtrasId) throws NotFoundException;

	/**
	 * @param  productId
	 * @param  activeRecords
	 * @return
	 * @throws NotFoundException
	 * @throws ValidationException
	 */
	List<ProductExtrasDTO> getListWithUserCheck(Long productId, Boolean activeRecords) throws NotFoundException, ValidationException;

	/**
	 * @param  activeRecords
	 * @param  productId
	 * @return
	 * @throws NotFoundException
	 */
	List<ProductExtrasDTO> getList(Boolean activeRecords, Long productId) throws NotFoundException;

	/**
	 * @param  activeRecords
	 * @param  productExtrasMasterId
	 * @return
	 * @throws NotFoundException
	 */
	List<ProductExtras> getListByProductExtrasMaster(Boolean activeRecords, Long productExtrasMasterId) throws NotFoundException;

}
