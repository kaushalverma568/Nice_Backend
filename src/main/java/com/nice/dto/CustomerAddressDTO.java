/**
 *
 */
package com.nice.dto;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

import lombok.Data;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 25-Jun-2020
 */
@Data
public class CustomerAddressDTO implements Serializable {
	/**
	*
	*/
	private static final long serialVersionUID = 511840781786542264L;

	private Long id;

	@NotNull(message = "{customer.id.not.null}")
	private Long customerId;

	@NotBlank(message = "{first.name.not.null}")
	private String firstName;

	@NotBlank(message = "{last.name.not.null}")
	private String lastName;

	@NotBlank(message = "{phone.number.not.null}")
	private String phoneNumber;

	@NotBlank(message = "{block.not.null}")
	private String block;

	@NotBlank(message = "{streetNo.not.null}")
	private String streetNo;

	@NotBlank(message = "{buildingName.not.null}")
	private String buildingName;

	@NotNull(message = "{area.id.not.null}")
	private Long areaId;

	private Long countryId;

	private Long stateId;

	private Long cityId;

	@NotNull(message = "{active.not.null}")
	private Boolean active;

	private boolean defaultAddress;

	@NotNull(message = "{latitude.not.null}")
	private BigDecimal latitude;

	@NotNull(message = "{longitude.not.null}")
	private BigDecimal longitude;

	/**
	 * it could be either Home or Work
	 */
	@NotBlank(message = "{addressOf.not.null}")
	private String addressOf;
}