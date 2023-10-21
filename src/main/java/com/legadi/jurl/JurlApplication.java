package com.legadi.jurl;

import static java.util.logging.Level.SEVERE;

import java.security.GeneralSecurityException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.logging.Logger;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import com.legadi.jurl.exception.CommandException;
import com.legadi.jurl.exception.InvalidAssertionsFoundException;
import com.legadi.jurl.exception.RequestException;
import com.legadi.jurl.exception.SkipExecutionException;
import com.legadi.jurl.executor.RequestCommand;

public class JurlApplication {

    private static final Logger LOGGER = Logger.getLogger(JurlApplication.class.getName());

    public static void main(String[] args) throws Exception {
        try {
            System.setProperty("java.util.logging.SimpleFormatter.format", "%5$s%6$s%n");
            System.setProperty("java.util.logging.ConsoleHandler.level", "INFO");

            setupConnectionstoAcceptAllHosts();

            new RequestCommand(args).execute();
        } catch(SkipExecutionException ex) {
            System.exit(0);
        } catch(InvalidAssertionsFoundException ex) {
            System.exit(1);
        } catch(CommandException | RequestException ex) {
            LOGGER.log(SEVERE, ex.getMessage());
            System.exit(1);
        } catch(Exception ex) {
            LOGGER.log(SEVERE, ex.getMessage(), ex);
            System.exit(1);
        }
    }

    private static void setupConnectionstoAcceptAllHosts() throws GeneralSecurityException {
        TrustManager[] trustAllCerts = new TrustManager[] {
            new X509TrustManager() {

                @Override
                public java.security.cert.X509Certificate[] getAcceptedIssuers() {
                    return null;
                }

                @Override
                public void checkClientTrusted(X509Certificate[] arg0, String arg1)
                    throws CertificateException {
                }

                @Override
                public void checkServerTrusted(X509Certificate[] arg0, String arg1)
                    throws CertificateException {
                }

            }
        };

        SSLContext sc = SSLContext.getInstance("SSL");
        sc.init(null, trustAllCerts, new java.security.SecureRandom());
        HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());

        HostnameVerifier validHosts = new HostnameVerifier() {

            @Override
            public boolean verify(String arg0, SSLSession arg1) {
                return true;
            }
        };

        HttpsURLConnection.setDefaultHostnameVerifier(validHosts);
    }

}
