package com.nice.service.impl;

import java.io.IOException;
import java.math.BigInteger;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.Constant;
import com.nice.constant.UserType;
import com.nice.dto.AddStockDto;
import com.nice.dto.AddStockRequestDTO;
import com.nice.dto.LotwiseStockRequestDTO;
import com.nice.dto.ProductVariantResponseDTO;
import com.nice.dto.StockDetailFilterDTO;
import com.nice.dto.StockDetailsDTO;
import com.nice.dto.StockTransferDto;
import com.nice.exception.FileNotFoundException;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.model.Product;
import com.nice.model.ProductVariant;
import com.nice.model.StockDetails;
import com.nice.model.StockTransfer;
import com.nice.model.UOM;
import com.nice.model.UserLogin;
import com.nice.model.Vendor;
import com.nice.repository.StockDetailsCustomRepository;
import com.nice.repository.StockDetailsRepository;
import com.nice.repository.StockTransferRepository;
import com.nice.service.ProductService;
import com.nice.service.ProductVariantService;
import com.nice.service.SchedulerDetailsService;
import com.nice.service.StockDetailsService;
import com.nice.service.UOMService;
import com.nice.util.ExportCSV;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 20-Jul-2020
 */
@Service("stockDetailsService")
@Transactional(rollbackFor = Throwable.class)
public class StockDetailsServiceImpl implements StockDetailsService {

	private static final Logger LOGGER = LoggerFactory.getLogger(StockDetailsServiceImpl.class);

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private StockDetailsRepository stockDetailsRepository;

	@Autowired
	private ProductService productService;

	@Autowired
	private UOMService uomService;

	@Autowired
	private ProductVariantService productVariantService;

	@Autowired
	private StockTransferRepository stockTransferRepository;

	@Autowired
	private StockDetailsCustomRepository stockDetailsCustomRepository;

	@Autowired
	private ExportCSV exportCSV;

	@Autowired
	private SchedulerDetailsService schedulerDetailsService;

	@Override
	public StockDetailsDTO addStockDetails(final AddStockDto addStockDto)
			throws ValidationException, NotFoundException {

		validateAddStock(addStockDto);
		/**
		 * method which find varient id from product id and UOM id
		 */
		ProductVariant productVariant = getproductVariantFromProductIdAndUomId(addStockDto.getProductId(),
				addStockDto.getUomId());

		StockDetails stockDetails = getStockDetailsForProductVarientAndLot(productVariant, addStockDto.getVendorId(),
				addStockDto.getLotNo());

		stockDetails.setAvailable(stockDetails.getAvailable() + addStockDto.getTotalQty());

		stockDetails.setActive(true);
		stockDetails.setExpiryDate(addStockDto.getExpiryDate());
		stockDetails.setLotDate(addStockDto.getLotDate());
		LOGGER.info("StockDetail Object to save :{}", stockDetails);
		stockDetails = stockDetailsRepository.save(stockDetails);
		/**
		 * Make an entry in the manual transaction table related to addition of new
		 * stock. Here direct repository call of manual stock transfer is made
		 */
		StockTransfer stockTransfer = new StockTransfer();
		stockTransfer.setActive(true);
		stockTransfer.setTransferedTo(Constant.AVAILABLE);
		stockTransfer.setQuantity(addStockDto.getTotalQty());
		stockTransfer.setStockDetails(stockDetails);
		stockTransfer.setVendorId(stockDetails.getVendorId());
		stockTransfer.setOrderId(1L);
		stockTransfer.setIsManual(true);
		LOGGER.info("ManualStockTransfer Object to save :{}", stockTransfer);
		stockTransferRepository.save(stockTransfer);
		/**
		 * Entry to make manual transaction table ends here
		 */
		return fetchStockInfo(stockDetails);
	}

	@Override
	public StockDetails updateStockDetails(final StockTransferDto stockTransferDto)
			throws NotFoundException, ValidationException {
		/**
		 * Validate the stockTransferDto
		 */
		validateStockTransfer(stockTransferDto);
		ProductVariant productVariant = getproductVariantFromProductIdAndUomId(stockTransferDto.getProductId(),
				stockTransferDto.getUomId());
		StockDetails stockDetails = getStockDetailsForProductVarientAndLot(productVariant,
				stockTransferDto.getVendorId(), stockTransferDto.getLotNo());
		/**
		 * Decide from where to subtract the qty
		 */
		substractQty(stockTransferDto, stockDetails);

		/**
		 * Decide from where to add the qty
		 */
		addQty(stockTransferDto, stockDetails);
		LOGGER.info("StockDetail Object to update :{}", stockDetails);
		return stockDetailsRepository.save(stockDetails);
	}

	private StockDetails addQty(final StockTransferDto stockTransferDto, final StockDetails stockDetails)
			throws ValidationException {
		if (Constant.AVAILABLE.equals(stockTransferDto.getTransferedTo())) {
			stockDetails.setAvailable(stockDetails.getAvailable() + stockTransferDto.getQuantity());
		} else if (Constant.RESERVED.equals(stockTransferDto.getTransferedTo())) {
			stockDetails.setReserved(stockDetails.getReserved() + stockTransferDto.getQuantity());
		} else if (Constant.DELIVERED.equals(stockTransferDto.getTransferedTo())) {
			stockDetails.setDelivered(stockDetails.getDelivered() + stockTransferDto.getQuantity());
		} else if (Constant.REPLACED.equals(stockTransferDto.getTransferedTo())) {
			stockDetails.setReplaced(stockDetails.getReplaced() + stockTransferDto.getQuantity());
		} else if (Constant.RETURNED.equals(stockTransferDto.getTransferedTo())) {
			stockDetails.setReturned(stockDetails.getReturned() + stockTransferDto.getQuantity());
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("stock.status.to.not.exists",
					new Object[] { stockTransferDto.getTransferedTo() }));
		}

		return stockDetails;
	}

	private StockDetails substractQty(final StockTransferDto stockTransferDto, final StockDetails stockDetails)
			throws ValidationException {
		if (Constant.AVAILABLE.equals(stockTransferDto.getTransferedFrom())) {
			if (stockDetails.getAvailable() < stockTransferDto.getQuantity()) {
				throw new ValidationException(
						messageByLocaleService.getMessage("insufficient.stock.available.for.product", null));
			}
			stockDetails.setAvailable(stockDetails.getAvailable() - stockTransferDto.getQuantity());
		} else if (Constant.RESERVED.equals(stockTransferDto.getTransferedFrom())) {
			if (stockDetails.getReserved() < stockTransferDto.getQuantity()) {
				throw new ValidationException(
						messageByLocaleService.getMessage("insufficient.stock.reserved.for.product", null));
			}
			stockDetails.setReserved(stockDetails.getReserved() - stockTransferDto.getQuantity());
		} else if (Constant.DELIVERED.equals(stockTransferDto.getTransferedFrom())) {
			if (stockDetails.getDelivered() < stockTransferDto.getQuantity()) {
				throw new ValidationException(
						messageByLocaleService.getMessage("insufficient.stock.delivered.for.product", null));
			}
			stockDetails.setDelivered(stockDetails.getDelivered() - stockTransferDto.getQuantity());
		} else if (Constant.REPLACED.equals(stockTransferDto.getTransferedFrom())) {
			if (stockDetails.getReplaced() < stockTransferDto.getQuantity()) {
				throw new ValidationException(
						messageByLocaleService.getMessage("insufficient.stock.replaced.for.product", null));
			}
			stockDetails.setReplaced(stockDetails.getReplaced() - stockTransferDto.getQuantity());
		} else if (Constant.RETURNED.equals(stockTransferDto.getTransferedFrom())) {
			if (stockDetails.getReturned() < stockTransferDto.getQuantity()) {
				throw new ValidationException(
						messageByLocaleService.getMessage("insufficient.stock.returned.for.product", null));
			}
			stockDetails.setReturned(stockDetails.getReturned() - stockTransferDto.getQuantity());
		} else {
			throw new ValidationException(messageByLocaleService.getMessage("stock.status.from.not.exists",
					new Object[] { stockTransferDto.getTransferedFrom() }));
		}
		return stockDetails;

	}

	/**
	 * @param stockTransferDto
	 * @throws ValidationException
	 */
	private void validateStockTransfer(final StockTransferDto stockTransferDto) throws ValidationException {
		LOGGER.info("Validating stockTransfer, details are : {}", stockTransferDto);
		if (stockTransferDto.getQuantity() == 0 && stockTransferDto.getQuantity() < 0) {
			LOGGER.error("Error Validating Stock Transfer");
			throw new ValidationException(
					messageByLocaleService.getMessage("update.stock.value.non.zero", new Object[] {}));
		} else if (stockTransferDto.getTransferedTo().equalsIgnoreCase(stockTransferDto.getTransferedFrom())) {
			LOGGER.error("Error Validating Stock Transfer as To and From are same");
			throw new ValidationException(messageByLocaleService.getMessage("moveTo.moveFrom.same", new Object[] {}));
		}
		LOGGER.info("Successfully Validated StockTransferDto");
	}

	private void validateAddStock(final AddStockDto addStockDto) throws ValidationException {
		LOGGER.info("Validating stockTransfer, details are : {}", addStockDto);
		if (addStockDto.getExpiryDate() == null) {
			LOGGER.error("Error Validating Stock as expiry date is null");
			throw new ValidationException(messageByLocaleService.getMessage("expiryDate.not.null", null));
		} else if (addStockDto.getLotDate() == null) {
			LOGGER.error("Error Validating Stock as lot date is null");
			throw new ValidationException(messageByLocaleService.getMessage("lotDate.not.null", null));
		} else if (addStockDto.getTotalQty() == null) {
			LOGGER.error("Error Validating Stock as stock is null");
			throw new ValidationException(messageByLocaleService.getMessage("update.stock.not.null", null));
		}
		LocalDate todayDate = (new Date()).toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
		LocalDate lotDate = addStockDto.getLotDate().toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
		LocalDate expiryDate = addStockDto.getExpiryDate().toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
		if (addStockDto.getTotalQty() == 0 && addStockDto.getTotalQty() < 0) {
			LOGGER.error("Error Validating Stock Transfer");
			throw new ValidationException(
					messageByLocaleService.getMessage("update.stock.value.non.zero", new Object[] {}));
		} else if (expiryDate.compareTo(todayDate) < 0) {
			LOGGER.error("Error Validating Stock Transfer as To and From are same");
			throw new ValidationException(messageByLocaleService.getMessage("expiryDate.futuredata", new Object[] {}));
		} else if (lotDate.compareTo(todayDate) > 0) {
			LOGGER.error("Error Validating Stock Transfer as Qty is 0");
			throw new ValidationException(messageByLocaleService.getMessage("lotDate.pastdate", new Object[] {}));
		} else if (addStockDto.getLotNo() == null) {
			LOGGER.error("Error Validating Stock Transfer as lot no is null");
			throw new ValidationException(messageByLocaleService.getMessage("lot.no.not.null", new Object[] {}));
		}
		LOGGER.info("Successfully Validated StockTransferDto");
	}

	@Override
	public StockDetailsDTO getStockDetails(final Long stockDetailsId) throws NotFoundException {
		return fetchStockInfo(getStockDetailsDetails(stockDetailsId));
	}

	@Override
	public StockDetails getStockDetailsDetails(final Long stockDetailsId) throws NotFoundException {
		return stockDetailsRepository.findById(stockDetailsId).orElseThrow(() -> new NotFoundException(
				messageByLocaleService.getMessage("stock.detail.not.found", new Object[] { stockDetailsId })));
	}

	@Override
	public List<StockDetails> getStockDetailsForProductVariant(final ProductVariant productvariant, final Long vendorId)
			throws NotFoundException, ValidationException {
		return stockDetailsRepository.findByProductVariantAndVendorId(productvariant, vendorId);
	}

	@Override
	public List<Long> getStockDetailsForSku(final String sku) throws NotFoundException {
		StockDetailFilterDTO stockDetailFilterDTO = new StockDetailFilterDTO();
		stockDetailFilterDTO.setSku(sku);
		stockDetailFilterDTO.setActive(true);
		return stockDetailsCustomRepository.getStockDetailsListForSerachString(null, null, stockDetailFilterDTO)
				.stream().map(StockDetails::getLotNo).collect(Collectors.toList());
	}

	/**
	 * @param userId
	 * @param lotNo
	 * @param readmadeProductVariant
	 * @return
	 */
	private StockDetails createDefaultStockDetailObject(final ProductVariant productvariant, final Long lotNo,
			final Long vendorId) {
		StockDetails stockDetail = new StockDetails();
		stockDetail.setProductVariant(productvariant);
		stockDetail.setLotNo(lotNo);
		stockDetail.setVendorId(vendorId);
		stockDetail.setAvailable(0D);
		stockDetail.setReserved(0D);
		stockDetail.setDelivered(0D);
		stockDetail.setReplaced(0D);
		stockDetail.setReturned(0D);
		stockDetail.setExpired(0D);
		stockDetail.setActive(true);
		stockDetail.setLotDate(new Date());
		stockDetail.setExpiryDate(new Date());
		return stockDetail;
	}

	@Override
	public StockDetailsDTO fetchStockInfo(final StockDetails stockDetails) throws NotFoundException {
		StockDetailsDTO stockDetailsDto = new StockDetailsDTO();
		BeanUtils.copyProperties(stockDetails, stockDetailsDto);
		ProductVariant productVariant = stockDetails.getProductVariant();
		stockDetailsDto.setProductId(productVariant.getProduct().getId());
		if (LocaleContextHolder.getLocale().getLanguage().equals("en")) {
			stockDetailsDto.setProductName(productVariant.getProduct().getNameEnglish());
			stockDetailsDto.setMeasurement(productVariant.getUom().getMeasurementEnglish());
		} else {
			stockDetailsDto.setProductName(productVariant.getProduct().getNameArabic());
			stockDetailsDto.setMeasurement(productVariant.getUom().getMeasurementArabic());
		}
		stockDetailsDto.setUomId(productVariant.getUom().getId());
		stockDetailsDto.setSku(productVariant.getSku());
		stockDetailsDto.setExpiryDate(stockDetails.getExpiryDate());
		stockDetailsDto.setLotDate(stockDetails.getLotDate());
		stockDetailsDto.setLotNo(stockDetails.getLotNo());

		stockDetailsDto.setProductVariantId(productVariant.getId());
		return stockDetailsDto;
	}

	public StockDetails toEntity(final StockDetailsDTO stockDetailsDto) {
		StockDetails stockDetails = new StockDetails();
		BeanUtils.copyProperties(stockDetailsDto, stockDetails);
		return stockDetails;
	}

	public List<StockDetailsDTO> toDtos(final List<StockDetails> stockDetailsList) throws NotFoundException {
		List<StockDetailsDTO> results = new ArrayList<>();
		for (StockDetails c : stockDetailsList) {
			results.add(fetchStockInfo(c));
		}
		return results;
	}

	@Override
	public void exportStockDetailsList(final HttpServletResponse httpServletResponse,
			final StockDetailFilterDTO stockDetailFilterDTO) throws NotFoundException, FileNotFoundException {
		List<StockDetailsDTO> stockDetailsDtoList;
		if (stockDetailFilterDTO != null) {
			stockDetailFilterDTO.setActive(true);
			stockDetailsDtoList = toDtos(
					stockDetailsCustomRepository.getStockDetailsListForSerachString(null, null, stockDetailFilterDTO));
		} else {
			stockDetailsDtoList = toDtos(stockDetailsRepository.findAll());
		}
		final Object[] stockHeaderField = new Object[] { "Name", "uom", "Current Stock", "SKU", "Reserved", "delivered",
				"replaced", "expiryDate", "lotDate" };
		final Object[] stockDataField = new Object[] { "productName", "uomLabel", "available", "sku", "reserved",
				"delivered", "replaced", "expiryDate", "lotDate" };
		try {
			exportCSV.writeCSVFile(stockDetailsDtoList, stockDataField, stockHeaderField, httpServletResponse);
		} catch (IOException e) {
			throw new FileNotFoundException(messageByLocaleService.getMessage("export.file.create.error", null));
		}

	}

	@Override
	public Page<StockDetailsDTO> getStockDetailsList(final Integer pageNumber, final Integer pageSize,
			final StockDetailFilterDTO stockDetailFilterDTO) throws NotFoundException, ValidationException {

		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("id"));
		stockDetailFilterDTO.setActive(true);
		List<StockDetails> stockDetailsList = stockDetailsCustomRepository
				.getStockDetailsListForSerachString((int) pageable.getOffset(), pageSize, stockDetailFilterDTO);
		Long totalCount = stockDetailsCustomRepository.getStockDetailsListForSerachStringCount(stockDetailFilterDTO);
		List<StockDetailsDTO> dtos = toDtos(stockDetailsList);
		return new PageImpl<>(dtos, pageable, totalCount);
	}

	@Override
	public Double getStockDetailsDtoForProductAndUom(final Long productId, final Long uomId, final Long vendorId)
			throws NotFoundException, ValidationException {
		ProductVariant productVariant = getproductVariantFromProductIdAndUomId(productId, uomId);
		List<StockDetails> stockDetailsList = null;
		if (vendorId == null) {
			throw new ValidationException(messageByLocaleService.getMessage("stock.detail.id.not.null", null));
		} else {
			stockDetailsList = getStockDetailsForProductVariant(productVariant, vendorId);
		}
		if (!stockDetailsList.isEmpty()) {
			Double available = 0.0;
			for (StockDetails stockDetails2 : stockDetailsList) {
				available = available + stockDetails2.getAvailable();
			}
			return available;
		} else {
			return 0.0;
		}
	}

	@Override
	public ProductVariant getproductVariantFromProductIdAndUomId(final Long productId, final Long uomId)
			throws NotFoundException {
		Product product = productService.getProductDetail(productId);
		UOM uom = uomService.getUOMDetail(uomId);
		Optional<ProductVariant> productVariant = productVariantService
				.getProductVariantDetailByProductAndUOMOptional(product, uom);
		if (productVariant.isPresent()) {
			return productVariant.get();
		} else {
			throw new NotFoundException(
					messageByLocaleService.getMessage("product.not.found", new Object[] { productId }));
		}

	}

	@Override
	public void addStockDetails(final AddStockRequestDTO addStockRequestDTO)
			throws NotFoundException, ValidationException {
		ProductVariant productVariant = productVariantService.getProductVariantDetailBySku(addStockRequestDTO.getSku(),
				addStockRequestDTO.getVendorId());
		for (LotwiseStockRequestDTO lotwiseStockRequestDTO : addStockRequestDTO.getStockRequestDTOs()) {
			AddStockDto addStockDto = convertLotWiseStockToAddStockDTO(lotwiseStockRequestDTO,
					addStockRequestDTO.getVendorId(), productVariant);
			addStockDetails(addStockDto);
		}
	}

	@Override
	public boolean validateAddStockDetails(final List<LotwiseStockRequestDTO> stockRequestDTOs, final Vendor vendor,
			final ProductVariant productVariant) throws ValidationException {
		Set<LotwiseStockRequestDTO> lotwiseStockRequestDTOs = new HashSet<>();
		lotwiseStockRequestDTOs.addAll(stockRequestDTOs);
		if (stockRequestDTOs.size() != lotwiseStockRequestDTOs.size()) {
			return true;
		}
		for (LotwiseStockRequestDTO lotwiseStockRequestDTO : stockRequestDTOs) {
			if (stockDetailsRepository.findByProductVariantAndVendorIdAndLotNo(productVariant, vendor.getId(),
					lotwiseStockRequestDTO.getLotNo()).isPresent()) {
				return true;
			}
		}
		return false;
	}

	private AddStockDto convertLotWiseStockToAddStockDTO(final LotwiseStockRequestDTO lotwiseStockRequestDTO,
			final Long vendorId, final ProductVariant productVariant) {
		AddStockDto addStockDto = new AddStockDto();
		addStockDto.setProductId(productVariant.getProduct().getId());
		addStockDto.setUomId(productVariant.getUom().getId());
		addStockDto.setExpiryDate(lotwiseStockRequestDTO.getExpiryDate());
		addStockDto.setLotDate(lotwiseStockRequestDTO.getLotDate());
		addStockDto.setLotNo(lotwiseStockRequestDTO.getLotNo());
		addStockDto.setVendorId(vendorId);
		addStockDto.setTotalQty(lotwiseStockRequestDTO.getTotalQty());
		return addStockDto;
	}

	@Override
	public StockDetails getStockDetailsForProductVarientAndLotAndVendor(final ProductVariant productVariant,
			final Long vendorId, final Long lotNo) throws NotFoundException {
		return stockDetailsRepository.findByProductVariantAndVendorIdAndLotNo(productVariant, vendorId, lotNo)
				.orElseThrow(() -> new NotFoundException(
						messageByLocaleService.getMessage("stock.detail.not.found.product.lotNo.vendor",
								new Object[] { productVariant.getId(), lotNo, vendorId })));
	}

	@Override
	public StockDetails getStockDetailsForProductVarientAndLot(final ProductVariant productvariant, final Long vendorId,
			final Long lotNo) throws NotFoundException, ValidationException {
		Optional<StockDetails> stockDetails = stockDetailsRepository.findByProductVariantAndLotNo(productvariant,
				lotNo);
		if (stockDetails.isPresent()) {
			return stockDetails.get();
		}
		/**
		 * Create a new object and insert it into database and return it back with
		 * default values of qty in all status as 0
		 */
		else {
			StockDetails stockDetail = createDefaultStockDetailObject(productvariant, lotNo, vendorId);
			return stockDetailsRepository.save(stockDetail);
		}
	}

	@Override
	public List<Long> getLotNoListByVendorAndProductVariant(final Long vendorId, final Long productVariantId)
			throws NotFoundException, ValidationException {
		ProductVariantResponseDTO variantResponseDTO = productVariantService.getProductVariant(productVariantId);
		StockDetailFilterDTO stockDetailFilterDTO = new StockDetailFilterDTO();
		stockDetailFilterDTO.setProductId(variantResponseDTO.getProductId());
		stockDetailFilterDTO.setVendorId(vendorId);
		stockDetailFilterDTO.setUomId(variantResponseDTO.getUomId());
		stockDetailFilterDTO.setActive(true);
		return stockDetailsCustomRepository.getStockDetailsListForSerachString(null, null, stockDetailFilterDTO)
				.stream().map(StockDetails::getLotNo).collect(Collectors.toList());
	}

	@Override
	public List<Long> getLotNosWithAvailableQtyFromProductVariant(final Long vendorId, final Long productVariantId) {
		return stockDetailsRepository.getLotNosWithQtyAvailable(vendorId, productVariantId);
	}

	@Override
	public void moveQtyToExpiredState(final Date runDate) {
		List<StockDetails> stockDetailsList = stockDetailsRepository
				.findByExpiryDateLessThanAndAvailableGreaterThan(runDate, 0D);
		for (StockDetails stockDetails : stockDetailsList) {
			stockDetails.setExpired(stockDetails.getAvailable());
			stockDetails.setAvailable(0D);
			stockDetailsRepository.save(stockDetails);
		}
		try {
			schedulerDetailsService.updateSchedulerDate(Constant.EXPIRE_STOCK_SCHEDULER);
		} catch (NotFoundException e) {
			LOGGER.info("Error while executing scheduler ,{}", e.getMessage());
		}
	}

	@Override
	public Long getCountForVariant(final ProductVariant productVariant) {
		Long availableProductCount = stockDetailsRepository.countAvailableQtyForProductVariant(productVariant);
		return availableProductCount == null ? 0 : availableProductCount;
	}

	@Override
	public void deleteStock(final Long productId, final Long uomId, final Long vendorId, final Long lotNo)
			throws NotFoundException, ValidationException {
		ProductVariant productVariant = getproductVariantFromProductIdAndUomId(productId, uomId);
		StockDetails stockDetail = getStockDetailsForProductVarientAndLot(productVariant, vendorId, lotNo);
		if (stockDetail.getReserved() > 0) {
			throw new ValidationException(messageByLocaleService.getMessage("delete.stock", null));
		}
		stockDetail.setActive(false);
		stockDetail.setAvailable(0D);
		stockDetail.setReserved(0D);
		stockDetail.setDelivered(0D);
		stockDetail.setReplaced(0D);
		stockDetail.setReturned(0D);
		stockDetail.setExpired(0D);
		stockDetailsRepository.save(stockDetail);

		StockTransfer stockTransfer = new StockTransfer();
		stockTransfer.setActive(true);
		stockTransfer.setTransferedFrom(Constant.AVAILABLE);
		stockTransfer.setQuantity(0L);
		stockTransfer.setStockDetails(stockDetail);
		stockTransfer.setVendorId(vendorId);
		stockTransfer.setOrderId(1L);
		stockTransfer.setIsManual(true);
		LOGGER.info("ManualStockTransfer Object to save :{}", stockTransfer);
		stockTransferRepository.save(stockTransfer);
	}

	@Override
	public StockDetails getStockDetailsByProductVariantAndLotNo(final Long productVariantId, final Long lotNo)
			throws NotFoundException, ValidationException {
		ProductVariant productVariant = productVariantService.getProductVariantDetail(productVariantId);
		ProductVariantResponseDTO productVariantResponse = productVariantService.getProductVariant(productVariantId);
		return stockDetailsRepository.findByProductVariantAndLotNo(productVariant, lotNo).orElseThrow(
				() -> new NotFoundException(messageByLocaleService.getMessage("stock.not.avaliable", new Object[] {
						productVariantResponse.getProductName(), productVariantResponse.getMeasurement(), lotNo })));
	}

	@Override
	public Page<ProductVariantResponseDTO> getLowStockProduct(Long vendorId, Integer pageNumber, Integer pageSize)
			throws ValidationException, NotFoundException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication()
				.getPrincipal()).getUser();
		if (UserType.VENDOR.name().equals(userLogin.getEntityType()) && vendorId.equals(userLogin.getEntityId())) {
			Pageable pageable = PageRequest.of(pageNumber - 1, pageSize);
			List<BigInteger> productVariantIdList = stockDetailsCustomRepository.getLowStockProductDetails(vendorId,
					(int) pageable.getOffset(), pageSize);
			List<ProductVariantResponseDTO> productVariantResponseDTOs = new ArrayList<>();
			for (BigInteger productVariantId : productVariantIdList) {
				productVariantResponseDTOs.add(productVariantService.getProductVariant(productVariantId.longValue()));
			}
			Long totalCount = stockDetailsCustomRepository.getLowStockProductDetailsCount(vendorId);
			return new PageImpl<>(productVariantResponseDTOs, pageable, totalCount);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
	}

	@Override
	public Page<StockDetailsDTO> getExpireStockDetails(Long vendorId, Integer pageNumber, Integer pageSize)
			throws ValidationException, NotFoundException {
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication()
				.getPrincipal()).getUser();
		if (UserType.VENDOR.name().equals(userLogin.getEntityType()) && vendorId.equals(userLogin.getEntityId())) {
			Pageable pageable = PageRequest.of(pageNumber - 1, pageSize);
			List<StockDetailsDTO> stockDetailsDTOs = new ArrayList<>();
			List<StockDetails> stockDetailsList = stockDetailsCustomRepository.getExpiredStockDetails(vendorId, (int) pageable.getOffset(), pageSize);
			Long totalCount = stockDetailsCustomRepository.getExpiredStockDetailsCount(vendorId);
			for (StockDetails stockDetails : stockDetailsList) {
				stockDetailsDTOs.add(fetchStockInfo(stockDetails));
			}
			return new PageImpl<>(stockDetailsDTOs, pageable, totalCount);
		} else {
			throw new ValidationException(messageByLocaleService.getMessage(Constant.UNAUTHORIZED, null));
		}
	}

}
