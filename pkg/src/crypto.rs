use aes_gcm::{
    aead::{Aead, KeyInit, OsRng},
    Aes256Gcm, AeadCore,
};
use sha2::{Digest, Sha256};

fn derive_key(secret: &str) -> [u8; 32] {
    let hash = Sha256::digest(secret.as_bytes());
    hash.into()
}

pub fn encrypt(plaintext: &str, secret: &str) -> Vec<u8> {
    let key = derive_key(secret);
    let cipher = Aes256Gcm::new_from_slice(&key).expect("invalid key length");
    let nonce = Aes256Gcm::generate_nonce(&mut OsRng);
    let ciphertext = cipher
        .encrypt(&nonce, plaintext.as_bytes())
        .expect("encryption failed");
    let mut result = nonce.to_vec();
    result.extend_from_slice(&ciphertext);
    result
}

pub fn decrypt(data: &[u8], secret: &str) -> Option<String> {
    if data.len() < 12 {
        return None;
    }
    let key = derive_key(secret);
    let cipher = Aes256Gcm::new_from_slice(&key).ok()?;
    let nonce = aes_gcm::Nonce::from_slice(&data[..12]);
    let plaintext = cipher.decrypt(nonce, &data[12..]).ok()?;
    String::from_utf8(plaintext).ok()
}
