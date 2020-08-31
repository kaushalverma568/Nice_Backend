/**
 *
 */
package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.CompanyDTO;
import com.nice.dto.CompanyResponseDTO;
import com.nice.model.Company;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Jun-2020
 */
@Component
public class CompanyMapper {

	public CompanyResponseDTO toDto(final Company company) {
		Locale locale = LocaleContextHolder.getLocale();
		CompanyResponseDTO companyResponseDTO = new CompanyResponseDTO();
		BeanUtils.copyProperties(company, companyResponseDTO);
		if (locale.getLanguage().equals("en")) {
			companyResponseDTO.setName(company.getNameEnglish());
			companyResponseDTO.setCompanyAddress(company.getCompanyAddressEnglish());
		} else {
			companyResponseDTO.setName(company.getNameArabic());
			companyResponseDTO.setCompanyAddress(company.getCompanyAddressArabic());
		}
		return companyResponseDTO;
	}

	public Company toEntity(final CompanyDTO companyDto) {
		Company company = new Company();
		BeanUtils.copyProperties(companyDto, company);
		return company;
	}

	public List<CompanyResponseDTO> toDtos(final List<Company> companies) {
		List<CompanyResponseDTO> results = new ArrayList<>();
		for (Company company : companies) {
			results.add(toDto(company));
		}
		return results;
	}

}