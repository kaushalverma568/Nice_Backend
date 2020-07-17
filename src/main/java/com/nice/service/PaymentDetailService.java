/**
 *
 */
package com.nice.service;

import javax.validation.Valid;

import com.nice.dto.PaymentDetailDTO;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 15-Jul-2020
 */
public interface PaymentDetailService {

	/**
	 * @param paymentDetailDTO
	 */
	Long addPaymentDetail(@Valid PaymentDetailDTO paymentDetailDTO);

	/**
	 * @param paymentDetailId
	 * @return
	 */
	PaymentDetailDTO getPaymentDetailDto(Long paymentDetailId);

}
