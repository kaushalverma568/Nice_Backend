package com.nice.service.impl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.nice.constant.AssetConstant;
import com.nice.dto.CompanyDTO;
import com.nice.dto.CompanyResponseDTO;
import com.nice.exception.FileOperationException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.CompanyMapper;
import com.nice.model.Company;
import com.nice.repository.CompanyRepository;
import com.nice.service.AssetService;
import com.nice.service.CompanyService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Service("companyService")
@Transactional(rollbackFor = Throwable.class)
public class CompanyServiceImpl implements CompanyService {
	/*
	 * by logging, display operation detail in console
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(CompanyServiceImpl.class);

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private CompanyRepository companyRepository;

	@Autowired
	private CompanyMapper companyMapper;

	@Autowired
	private AssetService assetService;

	@Override
	public void addCompany(final CompanyDTO companyDTO, final MultipartFile logo) throws ValidationException, NotFoundException, FileOperationException {
		if (companyRepository.count() >= 1) {
			LOGGER.error("Only One Company Details allowed");
			throw new ValidationException(messageByLocaleService.getMessage("company.count.exhausted", null));
		} else {
			Company company = companyMapper.toEntity(companyDTO);
			company.setCompanyImageName(assetService.saveAsset(logo, AssetConstant.COMPANY_DIR, 0));
			company.setCompanyImageOriginalName(logo.getOriginalFilename());
			companyRepository.save(company);
		}
	}

	@Override
	public void updateCompany(final CompanyDTO companyDTO, final MultipartFile logo) throws NotFoundException, ValidationException, FileOperationException {
		if (companyDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("company.id.not.null", null));
		} else {
			Company company = getCompanyDetail(companyDTO.getId());
			String oldImageName = company.getCompanyImageName();
			String oldOriginalName = company.getCompanyImageOriginalName();
			company = companyMapper.toEntity(companyDTO);
			if (logo != null) {
				assetService.deleteFile(oldImageName, AssetConstant.COMPANY_DIR);
				company.setCompanyImageName(assetService.saveAsset(logo, AssetConstant.COMPANY_DIR, 0));
				company.setCompanyImageOriginalName(logo.getOriginalFilename());
			} else {
				company.setCompanyImageName(oldImageName);
				company.setCompanyImageOriginalName(oldOriginalName);
			}
			companyRepository.save(company);

		}
	}

	@Override
	public CompanyResponseDTO getCompany(final Boolean isImageRequired) throws NotFoundException {
		List<Company> companies = companyRepository.findAll();
		if (companies.isEmpty()) {
			throw new NotFoundException(messageByLocaleService.getMessage("company.not.found", null));
		}
		Company company = companies.get(0);
		CompanyResponseDTO companyResponseDTO = companyMapper.toDto(company);
		/**
		 * flag is added for avoiding exception at the time of Jms email notification
		 */
		if (Boolean.TRUE.equals(isImageRequired)) {
			companyResponseDTO.setCompanyImage(assetService.getGeneratedUrl(company.getCompanyImageName(), AssetConstant.COMPANY_DIR));
		}
		return companyResponseDTO;
	}

	@Override
	public Company getCompanyDetail(final Long companyId) throws NotFoundException {
		return companyRepository.findById(companyId).orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("company.not.found", null)));
	}

}
