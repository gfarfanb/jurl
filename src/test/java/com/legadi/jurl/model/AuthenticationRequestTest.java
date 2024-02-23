package com.legadi.jurl.model;

import static com.legadi.jurl.model.AuthorizationType.BASIC;
import static com.legadi.jurl.model.AuthorizationType.TOKEN;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.legadi.jurl.model.http.HTTPRequestEntry;
import com.legadi.jurl.options.OptionsReader.OptionEntry;
import com.legadi.jurl.options.SetValueOption;

public class AuthenticationRequestTest {

    @Test
    public void setterGetterValidation() {
        AuthenticationRequest<HTTPRequestEntry> model = new AuthenticationRequest<>();

        model.setAuthRequestInputPath("src/test/resources/basic-functions.spec.http");
        model.setAuthRequestName("create");
        model.setAuthType(TOKEN);
        model.setAuthApi(new HTTPRequestEntry());
        model.setAuthRequest(new HTTPRequestEntry());

        List<OptionEntry> options = new LinkedList<>();
        options.add(new OptionEntry(new SetValueOption(), new String[] { "field", "value" }));
        model.setAuthOptions(options);

        Assertions.assertEquals("src/test/resources/basic-functions.spec.http", model.getAuthRequestInputPath());
        Assertions.assertEquals("create", model.getAuthRequestName());
        Assertions.assertEquals(TOKEN, model.getAuthType());
        Assertions.assertNotNull(model.getAuthApi());
        Assertions.assertNotNull(model.getAuthRequest());
        Assertions.assertEquals(1, model.getAuthOptions().size());
        Assertions.assertEquals(SetValueOption.class, model.getAuthOptions().get(0).getLeft().getClass());
    }

    @Test
    public void equalsValidation() {
        AuthenticationRequest<HTTPRequestEntry> model = new AuthenticationRequest<>();
        model.setAuthRequestInputPath("src/test/resources/basic-functions.spec.http");
        model.setAuthRequestName("create");
        model.setAuthType(TOKEN);

        Assertions.assertTrue(model.equals(model));
        Assertions.assertFalse(model.equals(null));
        Assertions.assertFalse(model.equals(new Object()));

        AuthenticationRequest<HTTPRequestEntry> other1 = new AuthenticationRequest<>();
        other1.setAuthRequestInputPath("src/test/resources/basic-functions.spec.http");
        other1.setAuthRequestName("create");
        other1.setAuthType(TOKEN);

        Assertions.assertTrue(model.equals(other1));

        AuthenticationRequest<HTTPRequestEntry> other2 = new AuthenticationRequest<>();

        Assertions.assertFalse(model.equals(other2));

        AuthenticationRequest<HTTPRequestEntry> other3 = new AuthenticationRequest<>();
        other3.setAuthRequestInputPath("src/test/resources/basic-functions.spec.http");

        Assertions.assertFalse(model.equals(other3));

        AuthenticationRequest<HTTPRequestEntry> other4 = new AuthenticationRequest<>();
        other4.setAuthRequestInputPath("src/test/resources/basic-functions.spec.http");
        other4.setAuthRequestName("create");

        Assertions.assertFalse(model.equals(other4));
    }

    @Test
    public void containsAuthentication() {
        AuthenticationRequest<HTTPRequestEntry> authToken = new AuthenticationRequest<>();
        authToken.setAuthRequestInputPath("src/test/resources/basic-functions.spec.http");
        authToken.setAuthRequestName("create");
        authToken.setAuthType(TOKEN);

        AuthenticationRequest<HTTPRequestEntry> authBasic = new AuthenticationRequest<>();
        authBasic.setAuthRequestInputPath("src/test/resources/basic-functions.spec.http");
        authBasic.setAuthRequestName("create");
        authBasic.setAuthType(BASIC);

        Set<AuthenticationRequest<?>> authRequests = new HashSet<>();
        authRequests.add(authToken);
        authRequests.add(authToken);
        authRequests.add(authBasic);
        authRequests.add(authBasic);

        Assertions.assertEquals(2, authRequests.size());

        AuthenticationRequest<HTTPRequestEntry> key = new AuthenticationRequest<>();
        key.setAuthRequestInputPath("src/test/resources/basic-functions.spec.http");
        key.setAuthRequestName("create");
        key.setAuthType(TOKEN);

        Assertions.assertTrue(authRequests.contains(key));

        AuthenticationRequest<HTTPRequestEntry> notFound = new AuthenticationRequest<>();
        notFound.setAuthRequestInputPath("src/test/resources/basic-functions.spec.http");
        notFound.setAuthRequestName("post");
        notFound.setAuthType(TOKEN);

        Assertions.assertFalse(authRequests.contains(notFound));
    }
}
