package com.nice.dto;

import lombok.Data;

import java.io.Serializable;
import java.math.BigDecimal;

@Data
public class CoordinatesDTO implements Serializable {

    private static final long serialVersionUID = 7992154691907146552L;

    private BigDecimal latitude;
    private BigDecimal longitude;

}
