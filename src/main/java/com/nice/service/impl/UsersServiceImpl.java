package com.nice.service.impl;

import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.constant.Role;
import com.nice.constant.UserType;
import com.nice.dto.UsersDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.UsersMapper;
import com.nice.model.UserLogin;
import com.nice.model.Users;
import com.nice.repository.UserLoginRepository;
import com.nice.repository.UsersRepository;
import com.nice.service.UserLoginService;
import com.nice.service.UsersService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Service(value = "usersService")
@Transactional(rollbackFor = Throwable.class)
public class UsersServiceImpl implements UsersService {

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private UsersRepository usersRepository;

	@Autowired
	private UsersMapper usersMapper;

	@Autowired
	private UserLoginService userLoginService;

	@Autowired
	private UserLoginRepository userLoginRepository;

	private static final Logger LOGGER = LoggerFactory.getLogger(UsersServiceImpl.class);

	@Override
	public Users addUsers(final UsersDTO usersDTO) throws NotFoundException, ValidationException {
		if (Role.getByValue(usersDTO.getRole()) == null) {
			throw new ValidationException(messageByLocaleService.getMessage("role.not.proper", null));
		}
		if (usersDTO.getPassword() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("password.required", null));
		}

		Users users = usersRepository.save(usersMapper.toEntity(usersDTO));
		UserLogin userLogin = new UserLogin();
		userLogin.setEntityId(users.getId());
		userLogin.setEntityType(UserType.USER.name());
		userLogin.setEmail(users.getEmail());
		userLogin.setPassword(usersDTO.getPassword());
		userLogin.setRole(usersDTO.getRole());
		userLogin.setActive(true);
		userLoginService.addUserLogin(userLogin);
		return users;
	}

	@Override
	public Users updateUsers(final UsersDTO usersDTO) throws ValidationException, NotFoundException {
		if (usersDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("users.id.not.null", null));
		}

		if (Role.getByValue(usersDTO.getRole()) == null) {
			throw new ValidationException(messageByLocaleService.getMessage("role.not.proper", null));
		}
		Users existingUsers = getUsersDetails(usersDTO.getId());
		/**
		 * Change UserLogin Role if Users role is changed
		 */
		if (!existingUsers.getRole().equals(usersDTO.getRole()) || !existingUsers.getEmail().equalsIgnoreCase(usersDTO.getEmail())) {
			Optional<UserLogin> optUserLogin = userLoginService.getUserLoginBasedOnEmailAndEntityType(existingUsers.getEmail(), UserType.USER.name());
			if (optUserLogin.isPresent()) {
				UserLogin userLogin = optUserLogin.get();
				userLogin.setRole(usersDTO.getRole());
				userLogin.setEmail(usersDTO.getEmail().toLowerCase());
				userLoginService.updateUserLogin(userLogin);
			}
		}

		return usersRepository.save(usersMapper.toEntity(usersDTO));
	}

	@Override
	public UsersDTO getUsers(final Long usersId) throws NotFoundException, ValidationException {
		UsersDTO usersDTO = usersMapper.toDto(getUsersDetails(usersId));
		usersDTO.setUserLoginId(userLoginService.getUserLoginBasedOnEntityIdAndEntityType(usersId, UserType.USER.name()).getId());
		return usersDTO;
	}

	/**
	 * @param usersId
	 * @return
	 * @throws NotFoundException
	 */
	@Override
	public Users getUsersDetails(final Long usersId) throws NotFoundException {
		return usersRepository.findById(usersId)
				.orElseThrow(() -> new NotFoundException(messageByLocaleService.getMessage("user.not.found", new Object[] { usersId })));
	}

	@Override
	public Page<Users> getUsersList(final Integer pageNumber, final Integer pageSize, final Boolean activeRecords) {
		Pageable pageable = PageRequest.of(pageNumber - 1, pageSize, Sort.by("id"));
		if (activeRecords != null) {
			return usersRepository.findAllByActive(activeRecords, pageable);
		} else {
			return usersRepository.findAll(pageable);
		}
	}

	@Override
	public void changeStatus(final Long usersId, final Boolean active) throws ValidationException, NotFoundException {
		Users existingUsers = getUsersDetails(usersId);
		LOGGER.info("Existing Users details {} ", existingUsers);
		if (active == null) {
			throw new ValidationException(messageByLocaleService.getMessage("active.not.null", null));
		} else if (existingUsers.getActive().equals(active)) {
			throw new ValidationException(
					messageByLocaleService.getMessage("user.active.deactive", new Object[] { (active.booleanValue() ? "active" : "deActive") }));
		} else {
			if (Boolean.FALSE.equals(active)) {
				LOGGER.info("Deactivate user login");
				Optional<UserLogin> optUserLogin = userLoginService.getUserLoginBasedOnEmailAndEntityType(existingUsers.getEmail(), UserType.USER.name());
				if (optUserLogin.isPresent()) {
					UserLogin userLogin = optUserLogin.get();
					userLogin.setActive(false);
					userLoginService.updateUserLogin(userLogin);
				}
			} else {
				LOGGER.info("Activate customer");
			}
			existingUsers.setActive(active);
			usersRepository.save(existingUsers);
		}
	}

	@Override
	public boolean isUserExists(final UsersDTO usersDTO) {
		/**
		 * if super admin contains same email then return true
		 */
		if (userLoginRepository.findByEmailIgnoreCaseAndEntityTypeIsNull(usersDTO.getEmail()).isPresent()) {
			return true;
		} else {
			if (usersDTO.getId() != null) {
				/**
				 * At the time of update is user with same name exist or not except it's own id
				 */
				return usersRepository.findByEmailAndIdNot(usersDTO.getEmail().toLowerCase(), usersDTO.getId()).isPresent();
			} else {
				/**
				 * At the time of create is user with same name exist or not
				 */
				return usersRepository.findByEmail(usersDTO.getEmail().toLowerCase()).isPresent();
			}
		}
	}
}
